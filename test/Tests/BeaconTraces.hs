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
  tests,
  testTrace
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
import Data.List (foldl')
import Test.Tasty

import Prelude as Haskell (Semigroup (..),IO)

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
  { mintBeacons :: [(TokenName,Integer)]
  , useMintRedeemer :: Bool
  , redeemerPkh :: PaymentPubKeyHash
  , userMustSign :: Bool
  } deriving (Generic,ToJSON,FromJSON,ToSchema)

type TraceSchema = Endpoint "mint-beacons" MintBeaconParams

mkSchemaDefinitions ''TraceSchema

-- | Needed so that a beacon is at the wrong address.
burnConfig :: EmulatorConfig
burnConfig = EmulatorConfig (Left $ Map.fromList wallets) def
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
-- Models
-------------------------------------------------
mintBeacon :: MintBeaconParams -> Contract () TraceSchema Text ()
mintBeacon MintBeaconParams{..} = do
  userPubKeyHash <- ownFirstPaymentPubKeyHash

  let beaconPolicyHash = mintingPolicyHash beaconPolicy
      beaconRedeemer 
        | useMintRedeemer = toRedeemer $ MintBeacon redeemerPkh
        | otherwise = toRedeemer BurnBeacon
      
      lookups = plutusV2MintingPolicy beaconPolicy
      tx' =
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          mintBeacons
        )
        -- | Must be signed by payment pubkey
        <> if userMustSign
           then mustBeSignedBy userPubKeyHash
           else mempty
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

-------------------------------------------------
-- Enpoints
-------------------------------------------------
endpoints :: Contract () TraceSchema Text ()
endpoints = awaitPromise mintBeacon' >> endpoints
  where
    mintBeacon' = endpoint @"mint-beacons" mintBeacon

-------------------------------------------------
-- Scenarios
-------------------------------------------------
mintTooManyBeaconsForUser :: EmulatorTrace ()
mintTooManyBeaconsForUser = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let userPubKeyHash = mockWalletPaymentPubKeyHash $ knownWallet 1
  callEndpoint @"mint-beacons" h1 $
    MintBeaconParams
      { mintBeacons = [(pubKeyAsToken userPubKeyHash,4)]
      , useMintRedeemer = True
      , redeemerPkh = userPubKeyHash
      , userMustSign = True
      }

mintBeaconWithoutUserSigning :: EmulatorTrace ()
mintBeaconWithoutUserSigning = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let userPubKeyHash = mockWalletPaymentPubKeyHash $ knownWallet 2
  callEndpoint @"mint-beacons" h1 $
    MintBeaconParams
      { mintBeacons = [(pubKeyAsToken userPubKeyHash,1)]
      , useMintRedeemer = True
      , redeemerPkh = userPubKeyHash
      , userMustSign = True
      }

mintAdditionalTokensForOtherUsers :: EmulatorTrace ()
mintAdditionalTokensForOtherUsers = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let userPubKeyHash1 = mockWalletPaymentPubKeyHash $ knownWallet 1
      userPubKeyHash2 = mockWalletPaymentPubKeyHash $ knownWallet 2
  callEndpoint @"mint-beacons" h1 $
    MintBeaconParams
      { mintBeacons = 
          [ (pubKeyAsToken userPubKeyHash1,1)
          , (pubKeyAsToken userPubKeyHash2,1)
          ]
      , useMintRedeemer = True
      , redeemerPkh = userPubKeyHash1
      , userMustSign = True
      }

mintUsingBurnRedeemer :: EmulatorTrace ()
mintUsingBurnRedeemer = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let userPubKeyHash = mockWalletPaymentPubKeyHash $ knownWallet 2
  callEndpoint @"mint-beacons" h1 $
    MintBeaconParams
      { mintBeacons = [(pubKeyAsToken userPubKeyHash,1)]
      , useMintRedeemer = False
      , redeemerPkh = userPubKeyHash
      , userMustSign = False
      }

burnOtherUserBeacons :: EmulatorTrace ()
burnOtherUserBeacons = do
  h2 <- activateContractWallet (knownWallet 1) endpoints

  let userPubKeyHash = mockWalletPaymentPubKeyHash $ knownWallet 1
  callEndpoint @"mint-beacons" h2 $
    MintBeaconParams
      { mintBeacons = [(pubKeyAsToken userPubKeyHash,-3)]
      , useMintRedeemer = False
      , redeemerPkh = userPubKeyHash
      , userMustSign = False
      }

-------------------------------------------------
-- Test Functions
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ burnConfig
  testGroup "Cardano-Address-Book"
    [ testGroup "Mint Beacon"
      [ checkPredicate "Fail if too many beacons minted for user"
          (Test.not assertNoFailedTransactions) mintTooManyBeaconsForUser
      , checkPredicate "Fail if pubkey of token name didn't sign"
          (Test.not assertNoFailedTransactions) mintBeaconWithoutUserSigning
      , checkPredicate "Fail if minting other user's beacons in addition to own"
          (Test.not assertNoFailedTransactions) mintAdditionalTokensForOtherUsers
      , checkPredicate "Fail if burn redeemer used to mint"
          (Test.not assertNoFailedTransactions) mintUsingBurnRedeemer
      ]
    , testGroup "Burn Beacon"
      [ checkPredicateOptions opts "Always allow burning"
          assertNoFailedTransactions burnOtherUserBeacons
      ]
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def burnConfig burnOtherUserBeacons