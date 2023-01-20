module CLI.Run
(
  runCommand,
) where

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL

import CardanoAddressBook
import CLI.Types
import CLI.Query

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  Beacon beaconCmd -> runBeaconCmd beaconCmd
  CreateEntry entry output -> runCreateEntry entry output
  QueryAddressBook pkh network output -> runQueryCmd pkh network output

runQueryCmd :: PaymentPubKeyHash -> Network -> Output -> IO ()
runQueryCmd pkh network output = do
  let beaconID = BeaconID (beaconSymbol, pubKeyAsToken pkh)
  book <- queryCardano beaconID network
  case output of
    StdOut -> BL.putStr $ encodePretty book
    File file -> do
      BL.writeFile file $ encodePretty book
      putStrLn "Address book saved successfully."

runCreateEntry :: AddressEntry -> Output -> IO ()
runCreateEntry entry output = case output of
  StdOut -> BL.putStr $ encodePretty entry
  File file -> do
    BL.writeFile file $ encodePretty entry
    putStrLn "Entry created successfully."

runBeaconCmd :: BeaconCmd -> IO ()
runBeaconCmd beaconCmd = case beaconCmd of
    CreateBeaconRedeemer beaconRedeemer file -> runCreateRedeemer beaconRedeemer file
    ExportBeaconPolicyScript file -> runExportPolicy file
  where
    runCreateRedeemer :: BeaconRedeemer -> FilePath -> IO ()
    runCreateRedeemer r file = do
      writeData file r
      putStrLn "Beacon redeemer created successfully."

    runExportPolicy :: FilePath -> IO ()
    runExportPolicy file = do
      res <- writeScript file beaconScript
      case res of
        Right _ -> putStrLn "Beacon policy script exported successfully."
        Left err -> putStrLn $ "There was an error: " <> show err