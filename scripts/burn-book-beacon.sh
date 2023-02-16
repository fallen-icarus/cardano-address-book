# There isn't really a need to do this but this helper script is provided anyway.
# The address book will still be accessible after the beacon is burned.
# No updates can be made until another beacon is minted.

# Variables
dir="../assets/plutus-files/"
tmpDir="../assets/tmp/"
bookBeaconPolicyFile="${dir}beacon.plutus"
bookBeaconRedeemerFile="${dir}burn.json"

ownerPubKeyHash=$(cat ../assets/wallets/01.pkh)

# Export the beacon policy
cardano-address-book beacon policy-script \
  --out-file $bookBeaconPolicyFile

# Get the beacon policy id
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $bookBeaconPolicyFile)

# Create the full beacon name
beacon="${beaconPolicyId}.${ownerPubKeyHash}"

# Create the beacon redeemer file
cardano-address-book beacon create-redeemer \
  --burn-beacon \
  --out-file $bookBeaconRedeemerFile

# Create and submit tx
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in f91c10b841170583a47ac9641ed7eda80166e6199d435e61636ab2d9997aa92c#0 \
  --mint "-1 ${beacon}" \
  --mint-script-file $bookBeaconPolicyFile \
  --mint-redeemer-file $bookBeaconRedeemerFile \
  --change-address $(cat ../assets/wallets/01.addr) \
  --tx-in-collateral d5046a4d5a9c0a0ec6a9eabd0eb1524d54c3473459889b67ec17604f3c2e861b#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"