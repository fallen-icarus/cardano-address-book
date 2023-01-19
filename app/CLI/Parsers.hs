module CLI.Parsers 
(
  parseCommand
) where

import Options.Applicative

import CLI.Types
import CardanoAddressBook

parseCommand :: Parser Command
parseCommand = hsubparser $
  command "beacon"
    (info parseBeaconCmd $ progDesc "Commands for using the beacons.") <>
  command "query-address-book"
    (info parseQueryAddressBook $ progDesc "Query the address book.")

parseBeaconCmd :: Parser Command
parseBeaconCmd = fmap Beacon . hsubparser $
  command "policy-script"
    (info pExportBeaconPolicy $ progDesc "Export the beacon policy script.") <>
  command "create-redeemer"
    (info pCreateBeaconRedeemer $ progDesc "Create a redeemer for minting/burning beacons.")

parseQueryAddressBook :: Parser Command
parseQueryAddressBook =
    QueryAddressBook
      <$> pPubKeyHash
      <*> pNetwork
      <*> pOutput
  where
    pNetwork :: Parser Network
    pNetwork = pMainnet <|> pPreProdTestnet
      where
        pMainnet :: Parser Network
        pMainnet = Mainnet <$> strOption
          (  long "mainnet"
          <> metavar "STRING"
          <> help "Query the mainnet using the Blockfrost Api with the supplied api key.")
        
        pPreProdTestnet :: Parser Network
        pPreProdTestnet = PreProdTestnet <$> strOption
          (  long "preprod-testnet"
          <> metavar "STRING"
          <> help "Query the preproduction testnet using the Blockfrost Api with the supplied api key.")


pExportBeaconPolicy :: Parser BeaconCmd
pExportBeaconPolicy = ExportBeaconPolicyScript <$> pOutputFile

pCreateBeaconRedeemer :: Parser BeaconCmd
pCreateBeaconRedeemer =
    CreateBeaconRedeemer
      <$> (pMint <|> pBurn)
      <*> pOutputFile
  where
    pMint :: Parser BeaconRedeemer
    pMint = MintBeacon <$> option (eitherReader readPubKeyHash)
      (  long "mint-beacon"
      <> metavar "STRING"
      <> help "Mint a beacon for the supplied payment pubkey hash."
      )

    pBurn :: Parser BeaconRedeemer
    pBurn = BurnBeacon <$> option (eitherReader readPubKeyHash)
      (  long "burn-beacon"
      <> metavar "STRING"
      <> help "Burn a beacon for the supplied payment pubkey hash."
      )

pPubKeyHash :: Parser PaymentPubKeyHash
pPubKeyHash = option (eitherReader readPubKeyHash)
  (  long "payment-key-hash" 
  <> metavar "STRING" 
  <> help "The payment key hash."
  )

pOutputFile :: Parser FilePath
pOutputFile = strOption
  (  long "out-file"
  <> metavar "FILE"
  <> help "The output file."
  <> completer (bashCompleter "file")
  )

pStdOut :: Parser Output
pStdOut = flag' StdOut
  (  long "stdout"
  <> help "Display to stdout."
  )

pOutput :: Parser Output
pOutput = pStdOut <|> File <$> pOutputFile