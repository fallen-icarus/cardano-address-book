{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Types where

import qualified Data.Map as Map
import Data.Aeson

import CardanoAddressBook (PaymentPubKeyHash,BeaconRedeemer)

data Output = StdOut | File !FilePath

data Command
  = Beacon BeaconCmd
  | CreateEntry !AddressEntry !Output
  | QueryAddressBook !PaymentPubKeyHash !Network !Output

data BeaconCmd
  = CreateBeaconRedeemer !BeaconRedeemer !FilePath
  | ExportBeaconPolicyScript !FilePath

data Network 
  = Mainnet !String  -- ^ Api key
  | PreProdTestnet !String  -- ^ Api key

type Alias = String
type FullAddress = String
newtype AddressEntry = AddressEntry { addressEntry :: Map.Map Alias FullAddress } deriving (Show)

instance ToJSON AddressEntry where
  toJSON (AddressEntry entry) =
    object [ "0" .= entry ]