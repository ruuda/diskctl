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

import Control.Monad (forM_)
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.UUID (UUID)
import Prelude hiding (lookup)
import System.IO (stderr)
import Toml (TomlCodec, (.=))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.UUID as UUID
import qualified System.Environment as Environment
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
    <> "Size:          " <> (show $ diskSizeBytes d) <> " bytes\n"
    <> "Price:         " <> (show $ diskPrice d)

diskCodec :: TomlCodec Disk
diskCodec = Disk
  <$> Toml.text  "label"         .= diskLabel
  <*> Toml.text  "model_name"    .= diskModelName
  <*> Toml.text  "serial_number" .= diskSerialNumber
  <*> Toml.day   "purchase_date" .= diskPurchaseDate
  <*> Toml.day   "wipe_date"     .= diskWipeDate
  <*> Toml.int   "size_bytes"    .= diskSizeBytes
  <*> eurosCodec "price_eur"     .= diskPrice

data Partition = Partition
  { partLabel    :: Text
  , partDisk     :: Text
  , partLuksUuid :: UUID
  } deriving (Show)

partitionCodec :: TomlCodec Partition
partitionCodec = Partition
  <$> Toml.text "label"     .= partLabel
  <*> Toml.text "disk"      .= partDisk
  <*> uuidCodec "luks_uuid" .= partLuksUuid

data Assignment = Assignment
  { asgPartition     :: Text
  , asgInstallDate   :: Day
  , asgUninstallDate :: Maybe Day
  } deriving (Show)

assignmentCodec :: TomlCodec Assignment
assignmentCodec = Assignment
  <$> Toml.text "partition"      .= asgPartition
  <*> Toml.day  "install_date"   .= asgInstallDate
  <*> Toml.dioptional (Toml.day "uninstall_date") .= asgUninstallDate

data Filesystem = Filesystem
  { fsLabel      :: Text
  , fsBtrfsUuid  :: UUID
  , fsDisks      :: [Assignment]
  } deriving (Show)

filesystemCodec :: TomlCodec Filesystem
filesystemCodec = Filesystem
  <$> Toml.text                 "label"      .= fsLabel
  <*> uuidCodec                 "btrfs_uuid" .= fsBtrfsUuid
  <*> Toml.list assignmentCodec "disks"      .= fsDisks

data Catalog = Catalog
  { catalogDisks       :: [Disk]
  , catalogPartitions  :: [Partition]
  , catalogFilesystems :: [Filesystem]
  }

instance Show Catalog where
  show c = ""
    <> "Disks:\n"
    <> (intercalate "\n\n" $ fmap show $ catalogDisks c) <> "\n\n"
    <> "Partitions:\n"
    <> (intercalate "\n" $ fmap show $ catalogPartitions c) <> "\n\n"
    <> "Filesystems:\n"
    <> (intercalate "\n" $ fmap show $ catalogFilesystems c)

catalogCodec :: TomlCodec Catalog
catalogCodec = Catalog
  <$> Toml.list diskCodec       "disk"       .= catalogDisks
  <*> Toml.list partitionCodec  "partition"  .= catalogPartitions
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

    partUuids  = fmap partLuksUuid $ catalogPartitions catalog
    fsUuids    = fmap fsBtrfsUuid $ catalogFilesystems catalog
    uuids      = partUuids <> fsUuids
    fsLabels   = fmap fsLabel $ catalogFilesystems catalog
    partLabels = fmap partLabel $ catalogPartitions catalog
    diskLabels = fmap diskLabel $ catalogDisks catalog
  in do
    -- Report as many errors as we can find at once.
    numErrors <- sum <$> sequence
      [ reportDuplicates "Error: Duplicate UUID: " uuids
      , reportDuplicates "Error: Duplicate filesystem label: " fsLabels
      , reportDuplicates "Error: Duplicate partition label: " partLabels
      , reportDuplicates "Error: Duplicate disk label: " diskLabels
      ]
    -- If there were any erros at all, abort; validation failed.
    case numErrors of
      0 -> pure ()
      _ -> System.exitFailure

main :: IO ()
main = do
  args <- Environment.getArgs
  catalog <- readCatalog $ head args
  validateCatalog catalog
  putStrLn $ show catalog
