module CLI.Query
(
  queryCardano
) where

import Servant.Client
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Exception

import CLI.Types
import CLI.BlockfrostApi

-- | Returned AddressEntry is the combination of all entries.
queryCardano :: BeaconID -> Network -> IO AddressEntry
queryCardano beaconID network = do
  manager' <- newManager tlsManagerSettings
  case network of
    PreProdTestnet apiKey' -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey = BlockfrostApiKey apiKey'
      res <- runClientM (queryBlockfrost apiKey beaconID) env
      case res of
        Right res' -> return res'
        Left err -> throw err
    Mainnet apiKey' -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-mainnet.blockfrost.io" 443 "api/v0") 
          apiKey = BlockfrostApiKey apiKey'
      res <- runClientM (queryBlockfrost apiKey beaconID) env
      case res of
        Right res' -> return res'
        Left err -> throw err