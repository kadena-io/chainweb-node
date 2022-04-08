{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wno-deprecations #-}

import Control.Monad
import Data.List
import qualified Data.Map as Map
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Text
import Distribution.Verbosity
import GHC.Conc(getNumProcessors)
import System.Directory
import System.FilePath

main :: IO ()
main = defaultMainWithHooks
    simpleUserHooks
        { hookedPrograms = [makeProgram]
        , confHook =
            \t f -> do
                lbi <- confHook simpleUserHooks t f
                let plat = hostPlatform lbi
                case plat of
                    -- don't even try building rocksdb on mac, homebrew version is new enough
                    Platform _ OSX -> pure lbi
                        { localPkgDescr = flip updatePackageDescription (localPkgDescr lbi) $ (,[]) $ Just emptyBuildInfo
                            { extraLibs = ["rocksdb"]
                            -- , includes = ["rocksdb/c.h"]
                            }
                        }
                    _ -> do
                        toplevel <- getCurrentDirectory
                        let rocksdb_tar = toplevel </> "rocksdb-6.29.3.tar.gz"
                        let
                            clbi = getComponentLocalBuildInfo lbi (CLibName LMainLibName)
                            -- rocksdb_libname =
                                -- getHSLibraryName (componentUnitId clbi) ++ "-rocksdb-6.29.3"
                        let
                            rocksdb_srcdir = "rocksdb-6.29.3"
                            extra_libs = ["stdc++", "gflags", "snappy", "bz2"]
                            builddir =
                                componentBuildDir lbi clbi
                        withCurrentDirectory builddir $ do
                            runLBIProgram lbi tarProgram ["-xzf", rocksdb_tar]
                            copyDirectoryRecursive minBound (rocksdb_srcdir </> "include") (toplevel </> "include")
                            -- TODO: do a recursive listing for the utilities/ folder's headers
                            nprocs <- getNumProcessors
                            let
                                dllFile pat = pat <.> dllExtension plat
                                staticLibFile pat = pat <.> staticLibExtension plat
                                jobs = max 2 $ min 4 $ nprocs
                            runLBIProgram lbi makeProgram
                                [ "-C", rocksdb_srcdir, "-j" <> show jobs
                                , "static_lib"
                                ]
                            copyFile (rocksdb_srcdir </> staticLibFile "librocksdb") (staticLibFile "libCrocksdb")
                            runLBIProgram lbi makeProgram
                                [ "-C", rocksdb_srcdir, "clean"
                                ]
                            writeFile (dllFile "librocksdb") ""
                            writeFile (dllFile "libCrocksdb") ""
                            writeFile (dllFile "librocksdb" <.> "6.29") ""
                            -- runLBIProgram lbi makeProgram
                                -- [ "-C", rocksdb_srcdir, "-j" <> show jobs
                                -- , "shared_lib"
                                -- ]
                            -- copyFile (rocksdb_srcdir </> dllFile "librocksdb") (dllFile "librocksdb")
                            -- copyFile (rocksdb_srcdir </> dllFile "librocksdb") (dllFile "libCrocksdb")
                            -- copyFile (rocksdb_srcdir </> dllFile "librocksdb") (dllFile "librocksdb" <.> "6.29")
                            includeFiles <-
                                withCurrentDirectory (rocksdb_srcdir </> "include") $ listDirectoryRecursive "rocksdb"
                            pure
                                lbi
                                { localPkgDescr =
                                    updatePackageDescription
                                    ( Just
                                        emptyBuildInfo
                                        { extraLibs = extra_libs
                                        , includeDirs = ["rocksdb-6.29.3/include"]
                                        , installIncludes = includeFiles
                                        , extraBundledLibs = ["Crocksdb"]
                                        }
                                    , []) $
                                    localPkgDescr lbi
                                }
        }

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive fp = do
    contents <- listDirectory fp
    isFile <- traverse (doesFileExist . (fp </>)) contents
    let (fmap fst -> files, fmap fst -> dirs) = partition snd $ zip contents isFile
    recs <- join <$> traverse (listDirectoryRecursive . (fp </>)) dirs
    return $ ((fp </>) <$> files) ++ recs

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
    , programPostConf = \_ p ->
        -- build rocksdb without -march=native, to avoid processor feature mismatches (illegal instruction errors)
        return p { programOverrideEnv = [("PORTABLE", Just "1")] }
    }
