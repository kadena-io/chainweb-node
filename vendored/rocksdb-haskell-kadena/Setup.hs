{-# language OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Wno-deprecations #-}

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Text
import Distribution.Verbosity
import System.Directory
import System.FilePath

main :: IO ()
main = defaultMainWithHooks
    simpleUserHooks
        { hookedPrograms = [makeProgram]
        , confHook =
            \t f -> do
                lbi <- confHook simpleUserHooks t f
                toplevel <- getCurrentDirectory
                let rocksdb_tar = toplevel </> "rocksdb-6.29.3.tar.gz"
                let
                    clbi = getComponentLocalBuildInfo lbi (CLibName LMainLibName)
                    -- rocksdb_libname =
                        -- getHSLibraryName (componentUnitId clbi) ++ "-rocksdb-6.29.3"
                let
                    rocksdb_srcdir = "rocksdb-6.29.3"
                    extra_libs = ["stdc++", "gflags"]
                    builddir =
                        componentBuildDir lbi clbi
                withCurrentDirectory builddir $ do
                    runLBIProgram lbi tarProgram ["-xzf", rocksdb_tar]
                    copyDirectoryRecursive minBound (rocksdb_srcdir </> "include") "include"
                    -- TODO: do a recursive listing for the utilities/ folder's headers
                    runLBIProgram lbi makeProgram ["-C", rocksdb_srcdir, "-j4", "static_lib", "shared_lib"]
                    copyFile (rocksdb_srcdir </> "librocksdb.so.6.29.3") "librocksdb.so"
                    copyFile (rocksdb_srcdir </> "librocksdb.so.6.29.3") "librocksdb.so.6.29.3"
                    copyFile (rocksdb_srcdir </> "librocksdb.so.6.29.3") "librocksdb.so.6.29"
                    copyFile (rocksdb_srcdir </> "librocksdb.so.6.29.3") "librocksdb.so.6"
                    copyFile (rocksdb_srcdir </> "librocksdb.so.6.29.3") "libCrocksdb.so"
                    copyFile (rocksdb_srcdir </> "librocksdb.a") "libCrocksdb.a"
                includeFiles <-
                    (fmap.fmap) ("rocksdb" </>) $
                        listDirectory (builddir </> "include" </> "rocksdb")
                -- remove directories
                rocksdbIncludes <- fmap catMaybes $ forM includeFiles $ \file -> do
                    e <- doesFileExist file
                    return $ file <$ guard e
                pure
                    lbi
                    { localPkgDescr =
                        updatePackageDescription
                        ( Just
                            emptyBuildInfo
                            { extraLibs = extra_libs
                            , includeDirs = [builddir </> "include"]
                            , installIncludes = rocksdbIncludes
                            }
                        , []) $
                        localPkgDescr lbi
                    }
        }

runLBIProgram :: LocalBuildInfo -> Program -> [ProgArg] -> IO ()
runLBIProgram lbi prog =
  runDbProgram
    (fromFlagOrDefault normal $ configVerbosity $ configFlags lbi)
    prog
    (withPrograms lbi)

-- getComponentLocalBuildInfo was removed in Cabal 3.
-- We simply define our own variant with the same flaws here,
-- but in terms of Cabal 3 definitions.
getComponentLocalBuildInfo :: LocalBuildInfo -> ComponentName -> ComponentLocalBuildInfo
getComponentLocalBuildInfo lbi cname =
    case Map.lookup cname (componentNameMap lbi) of
      Just [clbi] -> clbi
      Just clbis  ->
          error $ "internal error: the component name " ++ show cname
               ++ "is ambiguous.  Refers to: "
               ++ intercalate ", " (fmap (display . componentUnitId) clbis)
      Nothing ->
          error $ "internal error: there is no configuration data "
               ++ "for component " ++ show cname

makeProgram :: Program
makeProgram = (simpleProgram "make")
    { programFindVersion = findProgramVersion "--version" $ \str ->
        -- "GNU Make 4.3"
        case words str of
            (_:_:ver:_) -> ver
            _ -> ""
    }
