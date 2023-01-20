{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CLI.Types where

import qualified Data.Map as Map
import Data.Aeson

import CardanoAddressBook (PaymentPubKeyHash,BeaconRedeemer,CurrencySymbol,TokenName)

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
newtype AddressEntry = AddressEntry { addressEntry :: Map.Map Alias FullAddress } 
  deriving (Monoid,Semigroup,Show)

instance ToJSON AddressEntry where
  toJSON (AddressEntry entry) =
    object [ "0" .= entry ]

newtype BeaconID = BeaconID (CurrencySymbol,TokenName)