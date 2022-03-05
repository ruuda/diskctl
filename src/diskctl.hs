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

import Control.Monad (forM, forM_)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.List (intercalate, intersperse)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text.Buildable (Buildable (build))
import Data.Text.Format.Params (Params)
import Data.Text.Lazy.Builder (Builder)
import Data.Time.Calendar (Day)
import Data.UUID (UUID)
import Options.Applicative ((<**>))
import Prelude hiding (lookup)
import System.IO (hPutStrLn, stderr)
import Toml (TomlCodec, (.=))

import qualified Options.Applicative as Opts
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Format as Format
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Lazy as LazyText
import qualified Data.UUID as UUID
import qualified System.Exit as System
import qualified Toml

format :: Params ps => Format.Format -> ps -> Text
format f ps = LazyText.toStrict $ Format.format f ps

newtype Euros = Euros { euroCents :: Int } deriving Show

fromEuroCents :: Int -> Euros
fromEuroCents = Euros

instance Buildable Euros where
  build (Euros cents) = Format.build
    "€ {}.{}" (cents `div` 100, Format.left 2 '0' $ cents `mod` 100)

eurosCodec :: Toml.Key -> TomlCodec Euros
eurosCodec key = Toml.dimap
  ((/ 100.0) . fromIntegral . euroCents)
  (fromEuroCents . round . (* 100.0))
  (Toml.float key)

newtype Duration = Duration { durationSeconds :: Int } deriving Show

fromSeconds :: Int -> Duration
fromSeconds = Duration

instance Buildable Duration where
  build (Duration secs) = case secs of
    s | s < 60    -> Format.build "{}s"                                                                       (Format.Only s)
    s | s < 3600  -> Format.build "{}m:{}s"                                                           (s `div` 60, s `mod` 60)
    s | s < 86400 -> Format.build "{}h:{}m:{}s"                               (s `div` 3600, s `mod` 3600 `div` 60, s `mod` 60)
    s             -> Format.build "{}d:{}h:{}m:{}s" (s `div` 86400, s `mod` 86400 `div` 3600, s `mod` 3600 `div` 60, s `mod` 60)

formatByteSize :: Int -> Builder
formatByteSize sz = case sz of
  _ | sz <     1_000 -> Format.build "{} bytes" (Format.Only sz)

  _ | sz <    10_000 -> Format.build "{} kB" (Format.Only $ Format.fixed 3 $ fromIntegral sz / 1e3)
  _ | sz <   100_000 -> Format.build "{} kB" (Format.Only $ Format.fixed 2 $ fromIntegral sz / 1e3)
  _ | sz < 1_000_000 -> Format.build "{} kB" (Format.Only $ Format.fixed 1 $ fromIntegral sz / 1e3)

  _ | sz <    10_000_000 -> Format.build "{} MB" (Format.Only $ Format.fixed 3 $ fromIntegral sz / 1e6)
  _ | sz <   100_000_000 -> Format.build "{} MB" (Format.Only $ Format.fixed 2 $ fromIntegral sz / 1e6)
  _ | sz < 1_000_000_000 -> Format.build "{} MB" (Format.Only $ Format.fixed 1 $ fromIntegral sz / 1e6)

  _ | sz <    10_000_000_000 -> Format.build "{} GB" (Format.Only $ Format.fixed 3 $ fromIntegral sz / 1e9)
  _ | sz <   100_000_000_000 -> Format.build "{} GB" (Format.Only $ Format.fixed 2 $ fromIntegral sz / 1e9)
  _ | sz < 1_000_000_000_000 -> Format.build "{} GB" (Format.Only $ Format.fixed 1 $ fromIntegral sz / 1e9)

  _ | sz <    10_000_000_000_000 -> Format.build "{} TB" (Format.Only $ Format.fixed 3 $ fromIntegral sz / 1e12)
  _ | sz <   100_000_000_000_000 -> Format.build "{} TB" (Format.Only $ Format.fixed 2 $ fromIntegral sz / 1e12)
  _ | sz < 1_000_000_000_000_000 -> Format.build "{} TB" (Format.Only $ Format.fixed 1 $ fromIntegral sz / 1e12)

  _ -> Format.build "{} PB" (Format.Only $ Format.fixed 3 $ fromIntegral sz / (1e15 :: Double))

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
  } deriving (Show)

pricePerTb :: Disk -> Euros
pricePerTb disk =
   fromEuroCents $
     1_000_000_000_000
     * (euroCents $ diskPrice disk)
     `div` (diskSizeBytes disk)

instance AsDisplayTree Disk where
  asDisplayTree d =
    [ Node "model name"    (diskModelName d) []
    , Node "serial number" (diskSerialNumber d) []
    , Node "size"          (format "{}, {} bytes" (formatByteSize $ diskSizeBytes d, diskSizeBytes d)) []
    , Node "price"         (format "{}, {}/TB" (diskPrice d, pricePerTb d)) []
    , Node "purchase date" (Text.pack $ show $ diskPurchaseDate d) []
    , Node "wipe date"     (Text.pack $ show $ diskWipeDate d) []
    , Node "wipe time"
        (case diskWipeSeconds d of
          Just seconds -> format "{}, {}/s" (fromSeconds seconds, formatByteSize $ diskSizeBytes d `div` seconds)
          Nothing      -> "unknown"
        ) []
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

-- Catalog with an index on disk, volume, and filesystemd id. Ids have been
-- cross-checked by `validateCatalog`, such that lookups do not fail. (E.g. if a
-- volume references a disk by label, that disk does exist.)
data ValidCatalog = ValidCatalog
  { validCatalog               :: Catalog
  , validDisks                 :: HashMap Text Disk
  , validVolumes               :: HashMap Text Volume
  , validFilesystems           :: HashMap Text Filesystem
    -- For every disk by id, the volumes that use it.
  , validDiskVolumes           :: HashMap Text [Assignment]
  }

-- Return the elements that occur multiple times in the list.
nonUnique :: (Hashable a, Eq a) => [a] -> [a]
nonUnique = id
  . fmap fst
  . filter (\(_, count) -> count > 1)
  . HashMap.toList
  . HashMap.fromListWith (+)
  . fmap (\x -> (x, 1 :: Int))

-- Print errors and exit if the catalog contains errors.
validateCatalog :: Catalog -> IO ValidCatalog
validateCatalog catalog =
  let
    -- Report any duplicates among xs, returns the number of duplicates.
    reportDuplicates :: forall a. (Show a, Hashable a, Eq a) => String -> [a] -> IO Int
    reportDuplicates message xs = case nonUnique xs of
      []    -> pure 0
      dupes -> do
        forM_ dupes $ \x -> hPutStrLn stderr $ message <> (show x)
        pure $ length dupes

    -- Try a lookup of each reference produced by `getRefs` in `referenced`, if
    -- that fails, print an error to stderr.
    reportBrokenReferences
      :: (a -> Text)
      -> (a -> [Text])
      -> Format.Format
      -> [a]
      -> HashMap Text v
      -> IO Int
    reportBrokenReferences getName getRefs message xs referenced
      = fmap sum $ forM xs $ \x ->
          fmap sum $ forM (getRefs x) $ \ref ->
            case HashMap.lookup ref referenced of
              Just _  -> pure (0 :: Int)
              Nothing -> do
                TextIO.hPutStrLn stderr $ format message (getName x, ref)
                pure 1

    indexOn :: forall k v. (Hashable k, Eq k) => (v -> k) -> [v] -> HashMap k v
    indexOn key = HashMap.fromList . fmap (\x -> (key x, x))

    volumeUuids  = fmap volumeLuksUuid $ catalogVolumes catalog
    fsUuids      = fmap fsBtrfsUuid $ catalogFilesystems catalog
    uuids        = volumeUuids <> fsUuids
    fsLabels     = fmap fsLabel $ catalogFilesystems catalog
    volumeLabels = fmap volumeLabel $ catalogVolumes catalog
    diskLabels   = fmap diskLabel $ catalogDisks catalog
    diskSerials  = fmap diskSerialNumber $ catalogDisks catalog

    validated0 = ValidCatalog
      { validCatalog     = catalog
      , validDisks       = indexOn diskLabel $ catalogDisks catalog
      , validVolumes     = indexOn volumeLabel $ catalogVolumes catalog
      , validFilesystems = indexOn fsLabel $ catalogFilesystems catalog
      , validDiskVolumes = HashMap.empty
      }

    -- For every disk, find all assignments that it was ever used in.
    diskVolumes = HashMap.fromListWith (<>) $ fmap
      (\asg ->
        let
          volume = getVolume (asgVolume asg) validated0
          disk = getDisk (volumeDisk volume) validated0
        in
          (diskLabel disk, [asg])
      )
      (concatMap fsVolumes $ catalogFilesystems catalog)

    reportMultipleAssignments :: IO Int
    reportMultipleAssignments = fmap sum $ forM (HashMap.toList diskVolumes) $
      \(disk, asgs) -> case (filter (isNothing . asgUninstallDate)) asgs of
        []       -> pure 0
        asg : [] -> pure 0
        _        -> do
          TextIO.hPutStrLn stderr $ format "Error: Disk \"{}\" is used simultaneously in multiple filesystems through these volumes:" [disk]
          forM_ asgs $ \asg ->
            TextIO.hPutStrLn stderr $ format " - {}, installed {}" (asgVolume asg, asgInstallDate asg)
          pure 1

    validated1 = validated0
      { validDiskVolumes = HashMap.fromListWith (<>) $
          fmap (\asg ->
            let
              volume = getVolume (asgVolume asg) validated0
              disk = getDisk (volumeDisk volume) validated0
            in
              (diskLabel disk, [asg])
          ) (concatMap fsVolumes $ catalogFilesystems catalog)
      }
  in do
    -- Report as many errors as we can find at once.
    numErrors <- sum <$> sequence
      [ reportDuplicates "Error: Duplicate UUID: " uuids
      , reportDuplicates "Error: Duplicate filesystem label: " fsLabels
      , reportDuplicates "Error: Duplicate volume label: " volumeLabels
      , reportDuplicates "Error: Duplicate disk label: " diskLabels
      , reportDuplicates "Error: Duplicate disk serial number: " diskSerials
      , reportBrokenReferences volumeLabel (pure . volumeDisk)
          "Error: Volume \"{}\" references non-existing disk: {}"
          (catalogVolumes catalog) (validDisks validated0)
      , reportBrokenReferences fsLabel (fmap asgVolume . fsVolumes)
          "Error: Filesystem \"{}\" references non-existing volume: {}"
          (catalogFilesystems catalog) (validVolumes validated0)
      , reportMultipleAssignments
      ]
    -- If there were any erros at all, abort; validation failed.
    case numErrors of
      0 -> pure validated1
      _ -> System.exitFailure

getVolume :: Text -> ValidCatalog -> Volume
getVolume label catalog = case HashMap.lookup label $ validVolumes catalog of
  Just v  -> v
  Nothing -> error "Impossible, we validated all references."

getDisk :: Text -> ValidCatalog -> Disk
getDisk label catalog = case HashMap.lookup label $ validDisks catalog of
  Just d  -> d
  Nothing -> error "Impossible, we validated all references."

-- Data structure to help rendering nested key-value pairs as a tree. A node has
-- a key, a value, and possibly children.
data DisplayNode = Node Text Text [DisplayNode] | Spacer

maxKeyWidth :: [DisplayNode] -> Int
maxKeyWidth nodes = case nodes of
  [] -> 0
  (Node k _ children) : more -> maximum
    -- The colon takes one space as well.
    [ 1 + Text.length k
    -- 3 places for the "├─ "
    , 3 + maxKeyWidth children
    , maxKeyWidth more
    ]
  Spacer : more -> maxKeyWidth more

renderTree :: [DisplayNode] -> [Text]
renderTree nodes = renderWidth (maxKeyWidth nodes) nodes
  where
    renderKey w k v = case v of
      "" -> k
      _  -> Text.justifyLeft w ' ' $ k <> ":"
    renderWidth w = \case
      [] ->
        []
      (Node k v children) : [] ->
        ["└─ " <> (renderKey w k v) <> "  " <> v] <>
        (fmap ("   " <>) $ renderWidth (w - 3) children)
      (Node k v children) : more ->
        ["├─ " <> (renderKey w k v) <> "  " <> v] <>
        (fmap ("│  " <>) $ renderWidth (w - 3) children) <>
        (if null children then [] else ["│  "]) <>
        (renderWidth w more)
      Spacer : [] -> []
      Spacer : more -> "│" : renderWidth w more

class AsDisplayTree a where
  asDisplayTree :: a -> [DisplayNode]

displayFilesystemAsTree :: ValidCatalog -> Filesystem -> [Text]
displayFilesystemAsTree catalog fs =
  let
    assignmentNode asg =
      let
        volume = getVolume (asgVolume asg) catalog
        disk = getDisk (volumeDisk volume) catalog
      in
        Node "volume" (asgVolume asg) $
          [ Node "luks uuid" (Text.pack $ show $ volumeLuksUuid volume) []
          , Node "installed" (Text.pack $ show $ asgInstallDate asg) [] ] <>
          -- If the disk is no longer installed, we don't show its full details.
          -- If it is still installed, we show the disk here.
          case asgUninstallDate asg of
            Just date ->
              [ Node "uninstalled" (Text.pack $ show date) []
              , Node "disk" (volumeDisk volume) []
              ]
            Nothing -> [Node "disk" (volumeDisk volume) (asDisplayTree disk)]
    fsNodes =
      [ Node "btrfs uuid" (Text.pack $ show $ fsBtrfsUuid fs) []
      ] <> (fmap assignmentNode $ fsVolumes fs)
  in
    (" ● " <> fsLabel fs) : (fmap ("   " <>) $ renderTree fsNodes)

displayDiskAsTree :: ValidCatalog -> Disk -> [Text]
displayDiskAsTree catalog disk =
  let
    diskNodes = asDisplayTree disk
  in
    (" ● " <> diskLabel disk) : (fmap ("   " <>) $ renderTree diskNodes)

data Command
  = CmdServe
  | CmdFilesystem
  | CmdDisk
  | CmdDebug

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
    <> Opts.help "The path of the inventory TOML file to load"
    )
  <*> Opts.subparser
    (  Opts.command "serve"
         (Opts.info (pure CmdServe)      (Opts.progDesc "Serve an overview page over http"))
    <> Opts.command "filesystem"
         (Opts.info (pure CmdFilesystem) (Opts.progDesc "Print all filesystems"))
    <> Opts.command "fs"
         (Opts.info (pure CmdFilesystem) (Opts.progDesc "Alias for 'filesystem'"))
    <> Opts.command "disk"
         (Opts.info (pure CmdDisk)       (Opts.progDesc "Print all disks"))
    <> Opts.command "debug"
         (Opts.info (pure CmdDebug)      (Opts.progDesc "Debug-print the catalog"))
    )

main :: IO ()
main =
  let
    optsDesc = Opts.info (mainParser <**> Opts.helper)
      (  Opts.fullDesc
      <> Opts.progDesc "Inspect disk inventory."
      <> Opts.header "diskctl -- Inspect disk inventory"
      )

    printTreesWithBlankLine :: [[Text]] -> IO ()
    printTreesWithBlankLine groups
      = TextIO.putStrLn
      $ Text.intercalate "\n"
      $ concat
      $ intersperse [""] groups

  in do
    mainOpts   <- Opts.execParser optsDesc
    catalogRaw <- readCatalog $ mainFile mainOpts
    catalog    <- validateCatalog catalogRaw
    case mainCommand mainOpts of
      CmdServe ->
        putStrLn "not implemented"

      CmdFilesystem ->
        printTreesWithBlankLine $ fmap
          (displayFilesystemAsTree catalog)
          (catalogFilesystems $ validCatalog $ catalog)

      CmdDisk ->
        printTreesWithBlankLine $ fmap
          (displayDiskAsTree catalog)
          (catalogDisks $ validCatalog $ catalog)

      CmdDebug ->
        putStrLn $ show $ validCatalog $ catalog
