# There isn't really a need to do this but this helper script is provided anyway.
# The address book will still be accessible after the beacon is burned.
# No updates can be made until another beacon is minted.
# The pubkey of the beacon must sign the transaction.

# Variables
dir="../assets/plutus-files/"
tmpDir="../assets/tmp/"
bookBeaconPolicyFile="${dir}beacon.plutus"
bookBeaconRedeemerFile="${dir}burn.json"

ownerPubKeyHash=$(cat ../assets/wallets/01.pkh)

# Export the beacon policy
cabal run -v0 cardano-address-book -- beacon policy-script \
  --out-file $bookBeaconPolicyFile

# Get the beacon policy id
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $bookBeaconPolicyFile)

# Create the full beacon name
beacon="${beaconPolicyId}.${ownerPubKeyHash}"

# Create the beacon redeemer file
cabal run -v0 cardano-address-book -- beacon create-redeemer \
  --burn-beacon $ownerPubKeyHash \
  --out-file $bookBeaconRedeemerFile

# Create and submit tx
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in b56f9b3905d140eb11dfd6e03b02a3706061a4d92b005082148b0ab3e57e5b09#0 \
  --tx-in b56f9b3905d140eb11dfd6e03b02a3706061a4d92b005082148b0ab3e57e5b09#1 \
  --mint "-1 ${beacon}" \
  --mint-script-file $bookBeaconPolicyFile \
  --mint-redeemer-file $bookBeaconRedeemerFile \
  --change-address $(cat ../assets/wallets/01.addr) \
  --tx-in-collateral 4d199236a5276d4d1fed93b82d31295b6e4ac4089942a2b92ff25c603f64bcf3#1 \
  --testnet-magic 1 \
  --required-signer-hash $ownerPubKeyHash \
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