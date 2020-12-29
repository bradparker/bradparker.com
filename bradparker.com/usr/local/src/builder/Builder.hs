{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Builder
  ( build,
    inputFile,
    outputFile,
    copyFile,
    toFile,
    getDirectoryFiles,
    getConfig,
    Builder,
    Config (..),
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import qualified Data.ByteString.Lazy as LBS
import Options.Applicative as OptParse
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropDrive, takeDirectory, (</>))
import qualified System.FilePattern.Directory as Directory

data Config = Config
  { input :: FilePath,
    output :: FilePath
  }

configP :: OptParse.Parser Config
configP = Config <$> inputP <*> outputP
  where
    inputP :: OptParse.Parser FilePath
    inputP =
      OptParse.strOption
        ( OptParse.long "input"
            <> OptParse.short 'i'
            <> OptParse.metavar "DIRECTORY"
        )

    outputP :: OptParse.Parser FilePath
    outputP =
      OptParse.strOption
        ( OptParse.long "output"
            <> OptParse.short 'o'
            <> OptParse.metavar "DIRECTORY"
        )

getConfig :: IO Config
getConfig = OptParse.execParser (OptParse.info configP OptParse.fullDesc)

type Builder a = ReaderT Config IO a

build :: Config -> Builder a -> IO a
build config builder = runReaderT builder config

outputFile :: FilePath -> LBS.ByteString -> Builder ()
outputFile path content = do
  output <- asks (.output)
  let outputPath = output </> path
  liftIO do
    putStrLn ("Writing: " <> path)
    createDirectoryIfMissing True (takeDirectory outputPath)
    LBS.writeFile outputPath content

inputFile :: FilePath -> Builder LBS.ByteString
inputFile path = do
  input <- asks (.input)
  liftIO (LBS.readFile (input </> path))

copyFile :: FilePath -> Builder ()
copyFile path = outputFile path =<< inputFile path

toFile :: String -> LBS.ByteString -> Builder ()
toFile url = outputFile (dropDrive url </> "index.html")

getDirectoryFiles :: [String] -> Builder [FilePath]
getDirectoryFiles patterns = do
  input <- asks (.input)
  liftIO (Directory.getDirectoryFiles input patterns)
