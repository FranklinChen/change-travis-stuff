{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory.PathWalk
  ( WalkStatus(Continue, StopRecursing)
  , pathWalkInterruptible
  )
import System.FilePath.Posix (takeFileName, (</>))
import Data.Foldable (for_)
import System.Environment (getArgs)
import qualified Data.HashSet as HashSet
import Text.Regex.PCRE.Light (Regex, compile, utf8, multiline, match)
import qualified Data.ByteString as BS
import Data.Monoid ((<>))

main :: IO ()
main = do
  rootDir <- getRootDir
  pathWalkInterruptible rootDir $ \root _ files ->
    if HashSet.member (takeFileName root) dirsIgnored
      then pure StopRecursing
      else do
        for_ files $ \file -> do
          if file == ".travis.yml"
            then addSudoFalse (root </> file)
            else pure ()
        pure Continue

getRootDir :: IO FilePath
getRootDir = do
  args <- getArgs
  case args of
    [rootDir] -> pure rootDir
    _ -> fail "Usage: change-travis-stuff rootDir"

-- | Don't ever recurse into these directories.
dirsIgnored :: HashSet.HashSet FilePath
dirsIgnored =
  HashSet.fromList
  [ ".git"
  , "target"
  , "dist"
  , ".stack-work"
  , ".cabal-sandbox"
  , "node_modules"
  , "Contents"
  , "_darcs"
  , ".svn"
  , ".hg"
  , ".idea"
  , "src"
  ]

-- | Only add "sudo: false" if there was no sudo already.
--
-- Print report to stdout.
addSudoFalse :: FilePath -> IO ()
addSudoFalse path = do
  bytes <- BS.readFile path
  case match sudoRegex bytes [] of
    Just _ -> pure ()
    Nothing -> do
      putStrLn ("Adding sudo false to: " <> path)
      appendFile path "\nsudo: false\n"

-- | Do some parsing to make sure not to get random embedded,
-- commented out sudo directives.
sudoRegex :: Regex
sudoRegex = compile "^\\s*sudo\\s*:" [utf8, multiline]
