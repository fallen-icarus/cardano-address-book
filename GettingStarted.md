# Getting Started

:warning: Assumes a local PreProduction Testnet node running locally and `cardano-cli` installed since it is used to actually build and sign transactions.

Template bash scripts that follow these steps are available [here](scripts/).

---
## Table of Contents
- [Installing](#installing)
- [Minting an address book beacon and adding the first entry](#minting-an-address-book-beacon-and-adding-the-first-entry)
- [Adding an entry](#adding-an-entry)
- [Burn the address book beacon](#burn-the-address-book-beacon)

---
## Installing
Instructions are adapted from the [plutus-pioneers-program](https://github.com/input-output-hk/plutus-pioneer-program) week 1 exercise.

1. Install NixOS cross-referencing the following resources.
     - https://nixos.org/download.html
     - https://docs.plutus-community.com
     - A few resources to understand the what and why regarding NixOS
       - https://nixos.org/manual/nix/stable
       - https://serokell.io/blog/what-is-nix
2. Set-up IOHK binary caches [How to set up the IOHK binary caches](https://github.com/input-output-hk/plutus-apps#iohk-binary-cache). "If you do not do this, you will end up building GHC, which takes several hours. If you find yourself building GHC, *stop* and fix the cache."

3. After adding the cache, you will need to restart the nix service. This can be done by executing `sudo systemctl restart nix` or by restarting your machine. If the cache was configured properly, you should see a lot of `copying path ... from 'https://cache.iog.io'` when you execute `nix-shell` in the next step.

4. Execute the following:
```
git clone https://github.com/fallen-icarus/cardano-address-book
git clone https://github.com/input-output-hk/plutus-apps
cd plutus-apps
git checkout v1.0.0
nix-shell           # this may take a while the first time

# Your terminal should now have a nix-shell prompt

cd ../cardano-address-book
cabal clean
cabal update
cabal build all
```
The `cardano-address-book` CLI program should now be at `dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-address-book-0.1.0.0/x/cardano-address-book/build/cardano-address-book/cardano-address-book`. Move the program to somewhere in your $PATH.

You can now exit the nix-shell with `exit`.

All `cardano-address-book` subcommands have an associated `--help` option. The functionality is meant to feel like `cardano-cli`.

---
## Minting an address book beacon and adding the first entry
### 1. Export the beacon policy script
``` Bash
cardano-address-book beacon policy-script \
  --out-file beacon.plutus
```

### 2. Get the beacon policy id
``` Bash
cardano-cli transaction policyid \
  --script-file beacon.plutus
```

### 3. Create the beacon full name
``` Haskell
beacon = policyId ++ "." ++ paymentPubKeyHash
```

### 4. Create the beacon redeemer file
``` Bash
cardano-address-book beacon create-redeemer \
  --mint-beacon <payment_pubkey_hash> \
  --out-file mint.json
```

### 5. Create the first entry
``` Bash
cardano-address-book create-entry \
  --alias <alias1> \
  --address <alias1_address> \
  --alias <alias2> \
  --address <alias2_address> \
  --out-file entry.json
```
You can add as many `alias`/`address` pairs as you want.

### 6. Create and submit the tx
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_to_pay_fee> \
  --tx-out "$(cat pubkey.addr) + 2000000 lovelace + 1 <beacon_full_name>" \
  --mint "1 <beacon_full_name>" \
  --mint-script-file beacon.plutus \
  --mint-redeemer-file mint.json \
  --change-address $(cat pubkey.addr) \
  --tx-in-collateral <utxo_for_collateral> \
  --metadata-json-file entry.json \
  --required-signer-hash <payment_pubkey_hash> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file pubkey.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

## Adding an entry
### 1. Export the beacon policy script
``` Bash
cardano-address-book beacon policy-script \
  --out-file beacon.plutus
```

The script will not be executed; it is just needed to get the full beacon name for manually balancing the transaction.

### 2. Get the beacon policy id
``` Bash
cardano-cli transaction policyid \
  --script-file beacon.plutus
```

### 3. Create the beacon full name
``` Haskell
beacon = policyId ++ "." ++ paymentPubKeyHash
``` 

### 5. Create the new entry
``` Bash
cardano-address-book create-entry \
  --alias <alias1> \
  --address <alias1_address> \
  --out-file entry.json
```
If you use an alias already in the address book, the address book will update the alias to have the newly supplied address.

### 6. Create and submit tx
``` Bash
cardano-cli transaction build \
  --tx-in <utxo_to_pay_fee> \
  --tx-in <utxo_with_beacon> \
  --tx-out "$(cat pubkey.addr) 2000000 lovelace + 1 <beacon_full_name>" \
  --change-address $(cat pubkey.addr) \
  --metadata-json-file entry.json \
  --testnet-magic 1 \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file pubkey.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

## Burn the address book beacon
### 1. Export the beacon policy script
``` Bash
cardano-address-book beacon policy-script \
  --out-file beacon.plutus
```

### 2. Get the beacon policy id
``` Bash
cardano-cli transaction policyid \
  --script-file beacon.plutus
```

### 3. Create the beacon full name
``` Haskell
beacon = policyId ++ "." ++ paymentPubKeyHash
```

### 4. Create the beacon redeemer file
``` Bash
cardano-address-book beacon create-redeemer \
  --burn-beacon <payment_pubkey_hash> \
  --out-file burn.json
```

### 5. Create and submit the tx
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_to_pay_fee> \
  --tx-in <utxo_with_beacon> \
  --mint "-1 <beacon_full_name>" \
  --mint-script-file beacon.plutus \
  --mint-redeemer-file burn.json \
  --change-address $(cat pubkey.addr) \
  --tx-in-collateral <utxo_for_collateral> \
  --testnet-magic 1 \
  --required-signer-hash <payment_pubkey_hash> \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file pubkey.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```