module CLI.Run
(
  runCommand,
) where

import CardanoAddressBook
import CLI.Types

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  Beacon beaconCmd -> runBeaconCmd beaconCmd
  QueryAddressBook pkh network file -> return ()

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