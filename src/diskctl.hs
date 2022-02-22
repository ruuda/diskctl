-- Diskctl -- Disk inventory management system.
-- Copyright 2022 Ruud van Asseldonk

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Options.Applicative ((<**>))
import Control.Monad (forM_)
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.UUID (UUID)
import Prelude hiding (lookup)
import System.IO (stderr)
import Toml (TomlCodec, (.=))

import qualified Options.Applicative as Opts
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.UUID as UUID
import qualified System.Exit as System
import qualified Toml

newtype Euros = Euros { euroCents :: Int }

fromEuroCents :: Int -> Euros
fromEuroCents = Euros

instance Show Euros where
  -- TODO: Needs padding, use a string formatting library.
  show (Euros cents) = "€ " <> (show $ cents `div` 100) <> "." <> (show $ cents `mod` 100)

eurosCodec :: Toml.Key -> TomlCodec Euros
eurosCodec key = Toml.dimap
  ((/ 100.0) . fromIntegral . euroCents)
  (fromEuroCents . round . (* 100.0))
  (Toml.float key)

uuidCodec :: Toml.Key -> TomlCodec UUID
uuidCodec = Toml.textBy UUID.toText $ \t -> case UUID.fromText t of
  Nothing   -> Left $ "Invalid UUID: " <> t
  Just uuid -> Right uuid

data Disk = Disk
  { diskLabel        :: Text
  , diskModelName    :: Text
  , diskSerialNumber :: Text
  , diskPurchaseDate :: Day
  , diskWipeDate     :: Day
  , diskWipeSeconds  :: Maybe Int
  , diskSizeBytes    :: Int
  , diskPrice        :: Euros
  }

instance Show Disk where
  show d = ""
    <> "Label:         " <> (Text.unpack $ diskLabel d) <> "\n"
    <> "Model:         " <> (Text.unpack $ diskModelName d) <> "\n"
    <> "Serial number: " <> (Text.unpack $ diskSerialNumber d) <> "\n"
    <> "Purchase date: " <> (show $ diskPurchaseDate d) <> "\n"
    <> "Wipe date:     " <> (show $ diskWipeDate d) <> "\n"
    <> "Wipe time:     " <>
    ( case diskWipeSeconds d of
        Just seconds -> show seconds <> " seconds\n"
        Nothing      -> "unknown\n"
    )
    <> "Size:          " <> (show $ diskSizeBytes d) <> " bytes\n"
    <> "Price:         " <> (show $ diskPrice d)

instance AsDisplayTree Disk where
  asDisplayTree d =
    [ Node "label"         (diskLabel d) []
    , Node "model name"    (diskModelName d) []
    , Node "serial number" (diskSerialNumber d) []
    , Node "purchase date" (Text.pack $ show $ diskPurchaseDate d) []
    , Node "wipe date"     (Text.pack $ show $ diskWipeDate d) []
    , Node "wipe time"
        (case diskWipeSeconds d of
          Just seconds -> (Text.pack $ show seconds) <> " seconds"
          Nothing      -> "unknown"
        ) []
    , Node "size" ((Text.pack $ show $ diskSizeBytes d) <> " bytes") []
    , Node "price" (Text.pack $ show $ diskPrice d) []
    ]

diskCodec :: TomlCodec Disk
diskCodec = Disk
  <$> Toml.text  "label"         .= diskLabel
  <*> Toml.text  "model_name"    .= diskModelName
  <*> Toml.text  "serial_number" .= diskSerialNumber
  <*> Toml.day   "purchase_date" .= diskPurchaseDate
  <*> Toml.day   "wipe_date"     .= diskWipeDate
  <*> Toml.dioptional (Toml.int "wipe_seconds") .= diskWipeSeconds
  <*> Toml.int   "size_bytes"    .= diskSizeBytes
  <*> eurosCodec "price_eur"     .= diskPrice

data Volume = Volume
  { volumeLabel    :: Text
  , volumeDisk     :: Text
  , volumeLuksUuid :: UUID
  } deriving (Show)

volumeCodec :: TomlCodec Volume
volumeCodec = Volume
  <$> Toml.text "label"     .= volumeLabel
  <*> Toml.text "disk"      .= volumeDisk
  <*> uuidCodec "luks_uuid" .= volumeLuksUuid

data Assignment = Assignment
  { asgVolume        :: Text
  , asgInstallDate   :: Day
  , asgUninstallDate :: Maybe Day
  } deriving (Show)

assignmentCodec :: TomlCodec Assignment
assignmentCodec = Assignment
  <$> Toml.text "volume"       .= asgVolume
  <*> Toml.day  "install_date" .= asgInstallDate
  <*> Toml.dioptional (Toml.day "uninstall_date") .= asgUninstallDate

data Filesystem = Filesystem
  { fsLabel      :: Text
  , fsBtrfsUuid  :: UUID
  , fsVolumes    :: [Assignment]
  } deriving (Show)

filesystemCodec :: TomlCodec Filesystem
filesystemCodec = Filesystem
  <$> Toml.text                 "label"      .= fsLabel
  <*> uuidCodec                 "btrfs_uuid" .= fsBtrfsUuid
  <*> Toml.list assignmentCodec "volumes"    .= fsVolumes

data Catalog = Catalog
  { catalogDisks       :: [Disk]
  , catalogVolumes     :: [Volume]
  , catalogFilesystems :: [Filesystem]
  }

instance Show Catalog where
  show c = ""
    <> "Disks:\n"
    <> (intercalate "\n\n" $ fmap show $ catalogDisks c) <> "\n\n"
    <> "Volumes:\n"
    <> (intercalate "\n" $ fmap show $ catalogVolumes c) <> "\n\n"
    <> "Filesystems:\n"
    <> (intercalate "\n" $ fmap show $ catalogFilesystems c)

catalogCodec :: TomlCodec Catalog
catalogCodec = Catalog
  <$> Toml.list diskCodec       "disk"       .= catalogDisks
  <*> Toml.list volumeCodec     "volume"     .= catalogVolumes
  <*> Toml.list filesystemCodec "filesystem" .= catalogFilesystems

readCatalog :: FilePath -> IO Catalog
readCatalog fname = do
  Toml.decodeFileExact catalogCodec fname >>= \case
    Right catalog -> pure catalog
    Left msgs -> do
      -- Print errors to stderr, so we can still see them when piping stdout elsewhere.
      TextIO.hPutStrLn stderr $
        "Failed to parse " <> (Text.pack fname) <> ":\n"
        <> "  " <> Toml.prettyTomlDecodeErrors msgs
      System.exitFailure

-- Return the elements that occur multiple times in the list.
nonUnique :: (Hashable a, Eq a) => [a] -> [a]
nonUnique xs =
  let
    counts = HashMap.fromListWith (+) $ fmap (\x -> (x, 1 :: Int)) xs
  in
    [x | (x, count) <- HashMap.toList counts, count > 1]

-- Print errors and exit if the catalog contains errors.
validateCatalog :: Catalog -> IO ()
validateCatalog catalog =
  let
    -- Report any duplicates among xs, returns the number of duplicates.
    reportDuplicates :: forall a. (Show a, Hashable a, Eq a) => String -> [a] -> IO Int
    reportDuplicates message xs = case nonUnique xs of
      []    -> pure 0
      dupes -> do
        forM_ dupes $ \x -> putStrLn $ message <> (show x)
        pure $ length dupes

    volumeUuids  = fmap volumeLuksUuid $ catalogVolumes catalog
    fsUuids      = fmap fsBtrfsUuid $ catalogFilesystems catalog
    uuids        = volumeUuids <> fsUuids
    fsLabels     = fmap fsLabel $ catalogFilesystems catalog
    volumeLabels = fmap volumeLabel $ catalogVolumes catalog
    diskLabels   = fmap diskLabel $ catalogDisks catalog
    diskSerials  = fmap diskSerialNumber $ catalogDisks catalog
  in do
    -- Report as many errors as we can find at once.
    numErrors <- sum <$> sequence
      [ reportDuplicates "Error: Duplicate UUID: " uuids
      , reportDuplicates "Error: Duplicate filesystem label: " fsLabels
      , reportDuplicates "Error: Duplicate volume label: " volumeLabels
      , reportDuplicates "Error: Duplicate disk label: " diskLabels
      , reportDuplicates "Error: Duplicate disk serial number: " diskSerials
      ]
    -- If there were any erros at all, abort; validation failed.
    case numErrors of
      0 -> pure ()
      _ -> System.exitFailure

-- Data structure to help rendering nested key-value pairs as a tree. A node has
-- a key, a value, and possibly children.
data DisplayNode = Node Text Text [DisplayNode]

maxKeyWidth :: [DisplayNode] -> Int
maxKeyWidth nodes = case nodes of
  [] -> 0
  (Node k _ children) : more -> maximum
    [ Text.length k
    -- 3 places for the "├─ "
    , 3 + maxKeyWidth children
    , maxKeyWidth more
    ]

renderTree :: [DisplayNode] -> [Text]
renderTree nodes = renderWidth (maxKeyWidth nodes) nodes
  where
    renderWidth w = \case
      [] ->
        []
      (Node k v children) : [] ->
        ["└─ " <> (Text.justifyLeft w ' ' $ k <> ":") <> "  " <> v] <>
        (fmap ("   " <>) $ renderWidth (w - 3) children)
      (Node k v children) : more ->
        ["├─ " <> (Text.justifyLeft w ' ' $ k <> ":") <> "  " <> v] <>
        (fmap ("│  " <>) $ renderWidth (w - 3) children) <>
        (renderWidth w more)

class AsDisplayTree a where
  asDisplayTree :: a -> [DisplayNode]

data Command
  = CmdServe
  | CmdPrint

data MainOptions = MainOptions
  { mainFile :: FilePath
  , mainCommand :: Command
  }

mainParser :: Opts.Parser MainOptions
mainParser = MainOptions
  <$> Opts.strOption
    (  Opts.long "file"
    <> Opts.short 'f'
    <> Opts.metavar "<path>"
    <> Opts.help "The path of the inventory TOML file to load."
    )
  <*> Opts.subparser
    (  Opts.command "serve"
         (Opts.info (pure CmdServe) (Opts.progDesc "Serve an overview page over http."))
    <> Opts.command "print"
         (Opts.info (pure CmdPrint) (Opts.progDesc "Print the parsed version of the inventory."))
    )

main :: IO ()
main =
  let
    optsDesc = Opts.info (mainParser <**> Opts.helper)
      (  Opts.fullDesc
      <> Opts.progDesc "Inspect disk inventory."
      <> Opts.header "diskctl -- Inspect disk inventory."
      )
  in do
    mainOpts <- Opts.execParser optsDesc
    catalog <- readCatalog $ mainFile mainOpts
    validateCatalog catalog
    case mainCommand mainOpts of
      CmdPrint -> do
        forM_ (catalogDisks catalog) $ \disk ->
          TextIO.putStrLn $ Text.intercalate "\n" $ renderTree $ asDisplayTree disk
        putStrLn "---"
        putStrLn $ show catalog
      CmdServe -> putStrLn "not implemented"
