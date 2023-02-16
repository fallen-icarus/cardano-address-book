{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE BangPatterns          #-}

module CardanoAddressBook 
(
  readPubKeyHash,
  PaymentPubKeyHash,
  BeaconRedeemer (..),
  CurrencySymbol,
  TokenName,
  beaconScript,
  beaconSymbol,
  writeScript,
  writeData,

  -- For Testing
  beaconPolicy,
  pubKeyAsToken,
) where

import Data.Aeson hiding (Value)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import Prelude (IO,FilePath) 
import qualified Prelude as Haskell
import Data.String (fromString)

import           Cardano.Api hiding (Script,Value,TxOut)
import           Cardano.Api.Shelley   (PlutusScript (..))
import Plutus.V2.Ledger.Contexts
import Plutus.V2.Ledger.Api
import qualified PlutusTx
import PlutusTx.Prelude
import Ledger.Address
import Plutus.Script.Utils.V2.Scripts as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts
import Ledger.Bytes (fromHex)
import qualified Plutonomy
import Ledger.Value (flattenValue)
import qualified PlutusTx.AssocMap as Map

-------------------------------------------------
-- Misc Functions
-------------------------------------------------
-- | Parse PaymentPubKeyHash from user supplied String
readPubKeyHash :: Haskell.String -> Either Haskell.String PaymentPubKeyHash
readPubKeyHash s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ PaymentPubKeyHash $ PubKeyHash bytes'
  Left msg                   -> Left $ "could not convert: " <> msg

{-# INLINABLE pubKeyAsToken #-}
pubKeyAsToken :: PaymentPubKeyHash -> TokenName
pubKeyAsToken = TokenName . getPubKeyHash . unPaymentPubKeyHash

-------------------------------------------------
-- Address Book Beacon Settings
-------------------------------------------------
data BeaconRedeemer
  -- | To mint an address book beacon, the minted token must have the proper token name
  -- and the supplied pubkey must sign the tx.
  = MintBeacon PaymentPubKeyHash
  -- | While the beacon is not meant to constantly be minted and burned, this option is here just
  -- in case burning is needed.
  | BurnBeacon

PlutusTx.unstableMakeIsData ''BeaconRedeemer

-------------------------------------------------
-- On-Chain Address Book Beacon
-------------------------------------------------
mkBeacon :: BeaconRedeemer -> ScriptContext -> Bool
mkBeacon r ctx@ScriptContext{scriptContextTxInfo = info} = case r of
    MintBeacon pkh ->
      -- | Must mint one beacon with correct token name.
      mintCheck &&
      -- | Must be signed by the payment pubkey hash.
      traceIfFalse "Payment pubkey didn't sign." (txSignedBy info $ unPaymentPubKeyHash pkh)
    BurnBeacon ->
      -- | Proper beacon must be burned.
      mintCheck

  where
    beaconSym :: CurrencySymbol
    beaconSym = ownCurrencySymbol ctx

    -- | Returns only the beacons minted/burned
    beaconMint :: [(CurrencySymbol,TokenName,Integer)]
    beaconMint = case Map.lookup beaconSym $ getValue $ txInfoMint info of
      Nothing -> traceError "MintError"
      Just bs -> flattenValue $ Value $ Map.insert beaconSym bs Map.empty -- ^ a Value with only beacons

    mintCheck :: Bool
    mintCheck = case (r, beaconMint) of
      (MintBeacon pkh, [(_,tn,n)]) -> 
        let name = pubKeyAsToken pkh
        in if n < 1 then traceError "Must mint with this redeemer"
           else
             traceIfFalse "Can only mint beacon with user's pubkey as token name" (tn == name) &&
             traceIfFalse "One, and only one, beacon must be minted with this redeemer." (n == 1)
      (MintBeacon _, _) -> traceError "Can only mint beacon with user's pubkey as token name"
      (BurnBeacon, xs) ->
        traceIfFalse "Beacons can only be burned with this redeemer" (all (\(_,_,n) -> n < 0) xs)

beaconPolicy :: MintingPolicy
beaconPolicy = Plutonomy.optimizeUPLC $ mkMintingPolicyScript
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedMintingPolicy mkBeacon

beaconScript :: Script
beaconScript = unMintingPolicyScript beaconPolicy

beaconSymbol :: CurrencySymbol
beaconSymbol = scriptCurrencySymbol beaconPolicy

-------------------------------------------------
-- Serialization
-------------------------------------------------
dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file 
               . encode 
               . scriptDataToJson ScriptDataJsonDetailedSchema 
               . dataToScriptData 
               . PlutusTx.toData

serialisedScript :: Script -> PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise

writeScript :: FilePath -> Script -> IO (Either (FileError ()) ())
writeScript file script = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing
                        $ serialisedScript script

writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData = writeJSON