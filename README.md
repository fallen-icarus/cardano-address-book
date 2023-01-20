# Cardano-Address-Book

The Getting Started instructions can be found [here](GettingStarted.md).

---
## Motivation
Due to the seemingly random nature of blockchain addresses, it would be nice to be able to somehow save an "address book" so that users do not need to memorize the addresses they frequently use. The ideal solution would be to somehow link the address book with the user's payment key. This reasons for this are two-fold:

1. Every payment key would have its own address book. Users do not have to worry about if that address alias is taken by another user.
2. The address would always be recoverable as long as the payment key hash is known. The user can lose access to the payment key itself while still being able to see the address book (they just can't update it anymore).

## The Address Book Beacon
Linking the address book to the user's payment key is done by only allowing that user to mint the beacon for that payment key. The user's payment pubkey hash is used as the beacon's token name. In order to mint a beacon of a certain payment pubkey hash, two conditions must be met:

1. Only one beacon is minted in the transaction (there is no need for more than one).
2. The desired payment pubkey MUST SIGN the transaction.

By requiring the target payment pubkey to sign the transaction, no one but the owner of that payment pubkey can mint the address book beacon for that payment pubkey. Burning the address book beacon also requires the signature of the target payment pubkey.

## Using The Address Book Beacon
### Adding An Address Book Entry
Using the beacon to add an entry to the address book is very simple: **Just send the beacon to yourself and attach the address book entry as metadata to the transaction**. The beacon minting transaction can also include the very first address book entry.

An example entry:
``` JSON
{
    "0": {
        "User1": "addr_test1vrlfp27zjnjlsak5f7dnjkpl9ekeq5ezc3e4uw769y5rgtc4qvv2f",
        "User2": "addr_test1vzhq6qq52k59tekqp7v04yrpq284cqxjj7fx8qau2qd795s7wfhhm"
    }
}
```

The supplied `cardano-address-book` CLI program will properly format the entry for you so it is advized to rely on it.

### Querying The Address Book
Once the transaction is successfully added to the blockchain, you can then query the beacons off chain using two apis:

| Task | Koios Api | Blockfrost Api |
|--|--|--|
| Txs with the beacon | [api](https://api.koios.rest/#get-/asset_txs) | [api](https://docs.blockfrost.io/#tag/Cardano-Assets/paths/~1assets~1%7Basset%7D~1transactions/get)|
| Tx Metadata | [api](https://api.koios.rest/#post-/tx_metadata) | [api](https://docs.blockfrost.io/#tag/Cardano-Transactions/paths/~1txs~1%7Bhash%7D~1metadata/get)|

The `cardano-address-book query-address-book` subcommand does this for you. (It only supports Blockfrost right now since Koios does not have an api for the Preprod Testnet.) 

The returned address book is the combination of all address book entries submitted to the chain. For example, imagine you added these two entries to the chain (in separate transactions):

``` JSON
{
    "0": {
        "User1": "addr_test1vrlfp27zjnjlsak5f7dnjkpl9ekeq5ezc3e4uw769y5rgtc4qvv2f",
        "User2": "addr_test1vzhq6qq52k59tekqp7v04yrpq284cqxjj7fx8qau2qd795s7wfhhm"
    }
}
```

and

``` JSON
{
    "0": {
        "User3": "addr_test1vpz6g5ecxv6mc036lckg6w06wmj7vr073j73llzpsn5t0pguw7m5u"
    }
}
```

The returned address book would be:

``` JSON
{
    "0": {
        "User1": "addr_test1vrlfp27zjnjlsak5f7dnjkpl9ekeq5ezc3e4uw769y5rgtc4qvv2f",
        "User2": "addr_test1vzhq6qq52k59tekqp7v04yrpq284cqxjj7fx8qau2qd795s7wfhhm",
        "User3": "addr_test1vpz6g5ecxv6mc036lckg6w06wmj7vr073j73llzpsn5t0pguw7m5u"
    }
}
```

Note: if you try to query a beacon token that has never been minted before, you will get an api error when you try to query the beacon. This is due the the beacon itself being part of the Blockfrost api url.

### Updating The Address Book
Whenever you want to change the address associated to a certain alias, just add a new entry with the updated address for that alias. When the query is done off-chain, the newest entry always replaces the older one. 

Only the aliases being changed/added need to be submitted in any transaction. The off chain query will always be able to find all of the entries and properly aggregate them.

### Burning The Address Book Beacon
As previously stated, burning the beacon is possible as long as the related payment pubkey signs the transaction. There is no need to burn the beacon between uses. If you do burn the beacon, the address book cannot be updated again until the beacon is re-minted.

## Will This Cause Blockchain Bloat?
Each address entry is only about 100 bytes. I do not believe this will cause any issues related to blockchain bloat.