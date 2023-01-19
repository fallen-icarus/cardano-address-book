module CLI.Types where

import CardanoAddressBook (PaymentPubKeyHash,BeaconRedeemer)

data Output = StdOut | File !FilePath

data Command
  = Beacon BeaconCmd
  | QueryAddressBook !PaymentPubKeyHash !Network !Output

data BeaconCmd
  = CreateBeaconRedeemer !BeaconRedeemer !FilePath
  | ExportBeaconPolicyScript !FilePath

data Network 
  = Mainnet !String  -- ^ Api key
  | PreProdTestnet !String  -- ^ Api key