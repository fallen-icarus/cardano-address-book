{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}

module Tests.BeaconTraces
(
  test
) where

import Control.Lens
import qualified Data.Map as Map
import Data.Default
import Data.Void (Void)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Ledger hiding (singleton,mintingPolicyHash)
import Ledger.Constraints as Constraints
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), foldMap)
import Ledger.Value (singleton)
import Ledger.Ada (lovelaceValueOf)
import Plutus.Script.Utils.V2.Scripts as UScripts
import Playground.Contract (ToSchema)
import Playground.TH (mkSchemaDefinitions)
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty

import Prelude as Haskell (Semigroup (..), String)

import CardanoAddressBook

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
toRedeemer :: PlutusTx.ToData a => a -> Redeemer
toRedeemer = Redeemer . PlutusTx.dataToBuiltinData . PlutusTx.toData

-------------------------------------------------
-- Trace Configs
-------------------------------------------------
data MintBeaconParams = MintBeaconParams
  { mintPaymentPubKeyHash :: PaymentPubKeyHash
  , mintAmount :: Integer
  } deriving (Generic,ToJSON,FromJSON,ToSchema)

data BurnBeaconParams = BurnBeaconParams
  { burnPaymentPubKeyHash :: PaymentPubKeyHash
  , burnAmount :: Integer 
  } deriving (Generic,ToJSON,FromJSON,ToSchema)

type TraceSchema =
      Endpoint "mint-beacon" MintBeaconParams
  .\/ Endpoint "burn-beacon" BurnBeaconParams

mkSchemaDefinitions ''TraceSchema

-- | Needed so that a beacon is at the wrong address.
burnEmulatorConfig :: EmulatorConfig
burnEmulatorConfig = EmulatorConfig (Left $ Map.fromList wallets) def
  where
    beaconToken = 
      ( beaconSymbol
      , pubKeyAsToken $ mockWalletPaymentPubKeyHash $ knownWallet 1
      )

    user1 :: Value
    user1 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton beaconToken) 3

    user2 :: Value
    user2 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton beaconToken) 3
  
    wallets :: [(Wallet,Value)]
    wallets = 
      [ (knownWallet 1, user1)
      , (knownWallet 2, user2)
      ]

-------------------------------------------------
-- Testing Models
-------------------------------------------------
mintBeacon :: MintBeaconParams -> Contract () TraceSchema Text ()
mintBeacon MintBeaconParams{..} = do
  let beaconPolicyHash = mintingPolicyHash beaconPolicy
      beaconTokenName = pubKeyAsToken mintPaymentPubKeyHash
      beaconMintRedeemer = toRedeemer $ MintBeacon mintPaymentPubKeyHash

      lookups = plutusV2MintingPolicy beaconPolicy
      tx' =
        -- | Mint beacon
        mustMintCurrencyWithRedeemer beaconPolicyHash beaconMintRedeemer beaconTokenName mintAmount
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "minted an address book beacon"

burnBeacon :: BurnBeaconParams -> Contract () TraceSchema Text ()
burnBeacon BurnBeaconParams{..} = do
  let beaconPolicyHash = mintingPolicyHash beaconPolicy
      beaconTokenName = pubKeyAsToken burnPaymentPubKeyHash
      burnRedeemer = toRedeemer $ BurnBeacon burnPaymentPubKeyHash

  userPubKeyHash <- ownFirstPaymentPubKeyHash

  let lookups = plutusV2MintingPolicy beaconPolicy
      tx' =
        -- | Burn beacon
        mustMintCurrencyWithRedeemer beaconPolicyHash burnRedeemer beaconTokenName burnAmount
        -- | Must be signed by payment pubkey
        <> mustBeSignedBy userPubKeyHash
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "burned an address book beacon"

-------------------------------------------------
-- Enpoints
-------------------------------------------------
endpoints :: Contract () TraceSchema Text ()
endpoints = selectList choices >> endpoints
  where
    mintBeacon' = endpoint @"mint-beacon" mintBeacon
    burnBeacon' = endpoint @"burn-beacon" burnBeacon
    choices = 
      [ mintBeacon'
      , burnBeacon'
      ]

-------------------------------------------------
-- Trace Scenarios
-------------------------------------------------
-- | A trace where too many beacons are minted.
-- This should produce a failed transaction when mintBeacon is called.
mintTooManyBeacons :: EmulatorTrace ()
mintTooManyBeacons = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"mint-beacon" h1 $
    MintBeaconParams
      { mintPaymentPubKeyHash = mockWalletPaymentPubKeyHash $ knownWallet 1
      , mintAmount = 3
      }
  
  void $ waitUntilSlot 2

-- | A trace where the beacon does not go to the payment pubkey address.
-- This should produce a failed transaction when mintBeacon is called.
mintToTheWrongAddress :: EmulatorTrace ()
mintToTheWrongAddress = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"mint-beacon" h1 $
    MintBeaconParams
      { mintPaymentPubKeyHash = mockWalletPaymentPubKeyHash $ knownWallet 2
      , mintAmount = 1
      }
  
  void $ waitUntilSlot 2

-- | A trace where everything is correct.
-- This should produce a successfull transaction when mintBeacon is called.
successfullMint :: EmulatorTrace ()
successfullMint = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"mint-beacon" h1 $
    MintBeaconParams
      { mintPaymentPubKeyHash = mockWalletPaymentPubKeyHash $ knownWallet 1
      , mintAmount = 1
      }
  
  void $ waitUntilSlot 2

-- | A trace where too many beacons are burned.
-- This should produce a failed transaction when burnBeacon is called.
burnTooManyBeacons :: EmulatorTrace ()
burnTooManyBeacons = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"burn-beacon" h1 $
    BurnBeaconParams
      { burnPaymentPubKeyHash = mockWalletPaymentPubKeyHash $ knownWallet 1
      , burnAmount = (-3)
      }
  
  void $ waitUntilSlot 2

-- | A trace where the payment pubkey doesn't sign.
-- This should produce a failed transaction when burnBeacon is called.
pubKeyDoesntSign :: EmulatorTrace ()
pubKeyDoesntSign = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  callEndpoint @"burn-beacon" h2 $
    BurnBeaconParams
      { burnPaymentPubKeyHash = mockWalletPaymentPubKeyHash $ knownWallet 1
      , burnAmount = (-1)
      }
  
  void $ waitUntilSlot 2

-- | A trace where everything is correct.
-- This should produce a successfull transaction when burnBeacon is called.
successfullBurn :: EmulatorTrace ()
successfullBurn = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"burn-beacon" h1 $
    BurnBeaconParams
      { burnPaymentPubKeyHash = mockWalletPaymentPubKeyHash $ knownWallet 1
      , burnAmount = (-1)
      }
  
  void $ waitUntilSlot 2

test :: TestTree
test = do
  let burnOpts = defaultCheckOptions & emulatorConfig .~ burnEmulatorConfig
  testGroup "Cardano-Address-Book"
    [ testGroup "Mint Beacon"
      [ checkPredicate "Too Many Beacons Minted"
          (Test.not assertNoFailedTransactions) mintTooManyBeacons
      , checkPredicate "Beacon Minted To Wrong Address"
          (Test.not assertNoFailedTransactions) mintToTheWrongAddress
      , checkPredicate "Successfull Mint"
          assertNoFailedTransactions successfullMint
      ]
    , testGroup "Burn Beacon"
      [ checkPredicateOptions burnOpts "Too Many Beacons Burned"
          (Test.not assertNoFailedTransactions) burnTooManyBeacons
      , checkPredicateOptions burnOpts "PubKey Didn't Sign"
          (Test.not assertNoFailedTransactions) pubKeyDoesntSign
      , checkPredicateOptions burnOpts "Successfull Burn"
          assertNoFailedTransactions successfullBurn
      ]
    ]