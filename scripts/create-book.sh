# Variables
dir="../assets/plutus-files/"
tmpDir="../assets/tmp/"
bookBeaconPolicyFile="${dir}beacon.plutus"
bookBeaconRedeemerFile="${dir}mint.json"
addressEntryFile="${dir}entry.json"

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
  --mint-beacon $ownerPubKeyHash \
  --out-file $bookBeaconRedeemerFile

# Create the first entry file
cabal run -v0 cardano-address-book -- create-entry \
  --alias User2 \
  --address $(cat ../assets/wallets/02.addr) \
  --alias User3 \
  --address $(cat ../assets/wallets/03.addr) \
  --out-file $addressEntryFile

# Create and submit tx
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in f7ad4d14f64889547887cdc33173122352115f8e9b23187d494a301b7e9ca5f1#0 \
  --tx-out "$(cat ../assets/wallets/01.addr) + 2000000 lovelace + 1 ${beacon}" \
  --mint "1 ${beacon}" \
  --mint-script-file $bookBeaconPolicyFile \
  --mint-redeemer-file $bookBeaconRedeemerFile \
  --change-address $(cat ../assets/wallets/01.addr) \
  --tx-in-collateral 4d199236a5276d4d1fed93b82d31295b6e4ac4089942a2b92ff25c603f64bcf3#1 \
  --metadata-json-file $addressEntryFile \
  --required-signer-hash $ownerPubKeyHash \
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