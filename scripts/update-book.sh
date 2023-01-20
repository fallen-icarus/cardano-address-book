# In order to update the book, all you have to do is send the address book beacon to yourself
# and attach the new entry metadata file to the transaction.

# Variables
dir="../assets/plutus-files/"
tmpDir="../assets/tmp/"
addressEntryFile="${dir}entry.json"
bookBeaconPolicyFile="${dir}beacon.plutus"  # This is just needed to get the beacon policy id. It isn't executed.

ownerPubKeyHash=$(cat ../assets/wallets/01.pkh)

# Export the beacon policy
cabal run -v0 cardano-address-book -- beacon policy-script \
  --out-file $bookBeaconPolicyFile

# Get the beacon policy id
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $bookBeaconPolicyFile)

# Create the full beacon name
beacon="${beaconPolicyId}.${ownerPubKeyHash}"

# Create address entry
cabal run -v0 cardano-address-book -- create-entry \
  --alias User2 \
  --address $(cat ../assets/wallets/02.addr) \
  --alias User1 \
  --address $(cat ../assets/wallets/01.addr) \
  --out-file $addressEntryFile

# Create and submit tx
cardano-cli transaction build \
  --tx-in 94cb786f2253f6db72193b60d9b7b5b51d0c169c2273aed8d6f02a859e7af70a#0 \
  --tx-in 94cb786f2253f6db72193b60d9b7b5b51d0c169c2273aed8d6f02a859e7af70a#1 \
  --tx-out "$(cat ../assets/wallets/01.addr) + 2000000 lovelace + 1 ${beacon}" \
  --change-address $(cat ../assets/wallets/01.addr) \
  --metadata-json-file $addressEntryFile \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"