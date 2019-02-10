#!/usr/bin/env stack
{- stack
  --resolver nightly-2019-02-04
  --install-ghc
  runghc
  --package shake
  --package text
-}

{-# LANGUAGE OverloadedStrings #-}

import           Data.Char                  (isSpace)
import           Data.List                  (dropWhileEnd)
import           Data.Maybe                 (catMaybes)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Development.Shake
import           Development.Shake.FilePath

workflowFile :: FilePath
workflowFile = "Hoogle.alfredworkflow"

executableFile :: FilePath
executableFile = "alfred-hoogle"

main :: IO ()
main =
    shakeArgs shakeOptions
        $ action
        $ withTempDir
        $ \tempDir -> do

              -- Build the executable and copy to temp directory
              stackBuild
              binaryFp <- binaryPath
              copyFile' binaryFp $ tempDir </> executableFile

              -- Copy the plist to temp dir and set the version number
              copyFile' ("workflow_skeleton" </> "info.plist")
                        (tempDir </> "info.plist")
              ver <- cabalVer
              liftIO $ setPlistVersion ver $ tempDir </> "info.plist"

              -- Compress the skeleton directory
              command_ []
                       "zip"
                       ["-r", "-X", "-j", workflowFile, "workflow_skeleton"]

              -- Add the executable and updated plist file to the archive
              command_ [] "zip" ["-r", "-X", "-j", workflowFile, tempDir]


stackBuild :: Action ()
stackBuild = command_ [] "stack" ["build"]

binaryPath :: Action FilePath
binaryPath = do
    Stdout localbin <- command [] "stack" ["path", "--local-install-root"]
    return $ dropWhileEnd isSpace localbin </> "bin" </> executableFile

cabalVer :: Action Text
cabalVer = do
    Stdout ver <- command [] "stack" ["ls", "dependencies"]
    return $ defVer $ catMaybes $ T.stripPrefix "alfred-hoogle " <$> T.lines (T.pack ver)
      where defVer []    = "0.0.0"
            defVer (x:_) = x

setPlistVersion :: Text -> FilePath -> IO ()
setPlistVersion ver fp = do
    f <- TIO.readFile fp
    TIO.writeFile fp $ T.replace "VERSIONPLACEHOLDER" ver f
