-- Diskctl -- Disk inventory management system.
-- Copyright 2022 Ruud van Asseldonk

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.UUID (UUID)
import Prelude hiding (lookup)
import System.IO (stderr)
import Toml (TomlCodec, (.=))

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TextIO
import qualified System.Environment as Environment
import qualified System.Exit as System
import qualified Toml

newtype Euros = Euros { euroCents :: Int }

fromEuroCents :: Int -> Euros
fromEuroCents = Euros

instance Show Euros where
  show (Euros cents) = "€ " <> (show $ cents `div` 100) <> "." <> (show $ cents `mod` 100)

data Disk = Disk
  { diskLabel        :: Text
  , diskModelName    :: Text
  , diskSerialNumber :: Text
  , diskPurchaseDate :: Day
  , diskWipeDate     :: Day
  , diskSizeBytes    :: Int
  , diskPriceEur     :: Float
  }

diskCodec :: TomlCodec Disk
diskCodec = Disk
  <$> Toml.text  "label"         .= diskLabel
  <*> Toml.text  "model_name"    .= diskModelName
  <*> Toml.text  "serial_number" .= diskSerialNumber
  <*> Toml.day   "purchase_date" .= diskPurchaseDate
  <*> Toml.day   "wipe_date"     .= diskWipeDate
  <*> Toml.int   "size_bytes"    .= diskSizeBytes
  <*> Toml.float "price_eur"     .= diskPriceEur -- TODO: Wrapper type.

data Partition = Partition
  { partLabel    :: Text
  , partDisk     :: Text
  , partLuksUuid :: Text -- TODO: UUID type
  }

partitionCodec :: TomlCodec Partition
partitionCodec = Partition
  <$> Toml.text  "label"     .= partLabel
  <*> Toml.text  "disk"      .= partDisk
  <*> Toml.text  "luks_uuid" .= partLuksUuid

data Assignment = Assignment
  { asgPartition     :: Text
  , asgInstallDate   :: Day
  , asgUninstallDate :: Maybe Day
  }

maybeCodec :: TomlCodec a -> TomlCodec (Maybe a)
maybeCodec c = Toml.dimatch id Just c 

assignmentCodec :: TomlCodec Assignment
assignmentCodec = Assignment
  <$> Toml.text "partition"    .= asgPartition
  <*> Toml.day  "install_date" .= asgInstallDate
  <*> (maybeCodec $ Toml.day "uninstall_date") .= asgUninstallDate

data Filesystem = Filesystem
  { fsLabel      :: Text
  , fsBtrfsUuid  :: Text -- TODO: UUID
  , fsDisks      :: [Assignment]
  }

filesystemCodec :: TomlCodec Filesystem
filesystemCodec = Filesystem
  <$> Toml.text                 "label"      .= fsLabel
  <*> Toml.text                 "btrfs_uuid" .= fsBtrfsUuid
  <*> Toml.list assignmentCodec "disks"      .= fsDisks

data Catalog = Catalog
  { catalogDisks       :: [Disk]
  , catalogPartitions  :: [Partition]
  , catalogFilesystems :: [Filesystem]
  }

catalogCodec :: TomlCodec Catalog
catalogCodec = Catalog
  <$> Toml.list diskCodec       "disk"       .= catalogDisks
  <*> Toml.list partitionCodec  "partition"  .= catalogPartitions
  <*> Toml.list filesystemCodec "filesystem" .= catalogFilesystems

readCatalog :: FilePath -> IO (Either Text Catalog)
readCatalog fname =
  let
    decodeBytes tomlBytes = do
      tomlText <- case Text.decodeUtf8' tomlBytes of
        Right txt -> Right txt
        Left exc ->
          Left $ mempty
            <> "Failed to read " <> (Text.pack fname) <> " as UTF-8:\n"
            <> "  " <> (Text.pack $ show exc)

      case Toml.decode catalogCodec tomlText of
        Right catalog -> Right catalog
        Left msgs ->
          Left $ mempty
            <> "Failed to parse " <> (Text.pack fname) <> ":\n"
            <> "  " <> Toml.prettyTomlDecodeErrors msgs
  in
    fmap decodeBytes $ ByteString.readFile fname

main :: IO ()
main = do
  args <- Environment.getArgs
  catalog <- readCatalog (head args) >>= \case
    Right catalog -> pure catalog
    Left error -> do
      -- Print errors to stderr, so we can still see them when piping stdout elsewhere.
      TextIO.hPutStrLn stderr error
      System.exitFailure
  putStrLn "the end"
