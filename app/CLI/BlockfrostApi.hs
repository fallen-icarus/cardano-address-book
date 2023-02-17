{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module CLI.BlockfrostApi
(
  queryBlockfrost,
  BlockfrostApiKey (..),
) where

import Servant.API
import Servant.Client
import qualified Data.Text as T
import Data.Aeson
import Control.Monad (mzero)
import Data.Proxy

import CLI.Types (BeaconID(..),AddressEntry(..))

-- | Newtype wrapper around api key for using blockfrost
newtype BlockfrostApiKey = BlockfrostApiKey String

instance ToHttpApiData BlockfrostApiKey where
  toQueryParam (BlockfrostApiKey apiKey) = T.pack apiKey

instance ToHttpApiData BeaconID where
  toQueryParam (BeaconID (currSym,tokName)) = T.pack $ show currSym <> (drop 2 $ show tokName)

-- | The response from the asset transaction query
newtype AssetTxHash = AssetTxHash { unAssetTxHash :: String }

instance ToHttpApiData AssetTxHash where
  toQueryParam = T.pack . unAssetTxHash

instance FromJSON AssetTxHash where
  parseJSON (Object o) = AssetTxHash <$> o .: "tx_hash"
  parseJSON _ = mzero

instance FromJSON AddressEntry where
  parseJSON (Object o) = AddressEntry <$> o .: "json_metadata"
  parseJSON _ = mzero

-------------------------------------------------
-- Blockfrost Api
-------------------------------------------------
type BlockfrostApi
  =    "assets"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "asset" BeaconID
    :> "transactions"
    :> Get '[JSON] [AssetTxHash]

  :<|> "txs"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "hash" AssetTxHash
    :> "metadata"
    :> Get '[JSON] [AddressEntry]
  
beaconTxsApi :<|> addressEntryApi = client api
  where
    api :: Proxy BlockfrostApi
    api = Proxy

-------------------------------------------------
-- Query Blockfrost Function
-------------------------------------------------
-- | The AddressEntry returned is the combination of all AddressEntrys found
queryBlockfrost :: BlockfrostApiKey -> BeaconID -> ClientM AddressEntry
queryBlockfrost apiKey beacon = do
  txs <- beaconTxsApi apiKey beacon
  entries <- mapM (addressEntryApi apiKey) txs
  return $ (mconcat . mconcat) $ reverse entries