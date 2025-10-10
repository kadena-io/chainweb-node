-- ------------------------------------------------------ --
-- Copyright © 2019-2023 Kadena LLC <chainweb-dev@kadena.io>
-- Copyright © 2019 Colin Woodbury <colin@fosskers.ca>
-- Copyright © 2015-2018 Lars Kuhtz <lakuhtz@gmail.com>
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- | This module contains a @Setup.hs@ script that hooks into the cabal build
-- process at the end of the configuration phase and generates a module with
-- package information for each component of the cabal package.
--
-- The modules are created in the /autogen/ build directories where also the
-- @Path_@ modules are created by cabal's simple build setup.
--
-- = Usage as Setup Script
--
-- There are three ways how this module can be used:
--
-- 1. Copy the code of this module into a file called @Setup.hs@ in the root
--    directory of your package.
--
-- 2. If the /configuration-tools/ package is already installed in the system
--    where the build is done, following code can be used as @Setup.hs@ script:
--
--    > module Main (main) where
--    >
--    > import Configuration.Utils.Setup
--
-- 3. For usage within a more complex @Setup.hs@ script you shall import this
--    module qualified and use the 'mkPkgInfoModules' function. For example:
--
--    > module Main (main) where
--    >
--    > import qualified Configuration.Utils.Setup as ConfTools
--    >
--    > main :: IO ()
--    > main = defaultMainWithHooks (ConfTools.mkPkgInfoModules simpleUserHooks)
--    >
--
-- With all methods the field @Build-Type@ in the package description (cabal) file
-- must be set to @Custom@:
--
-- > Build-Type: Custom
--
--
-- = Integration With "Configuration.Utils"
--
-- You can integrate the information provided by the @PkgInfo@ modules with the
-- command line interface of an application by importing the respective module
-- for the component and using the
-- 'Configuration.Utils.runWithPkgInfoConfiguration' function from the module
-- "Configuration.Utils" as show in the following example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE FlexibleInstances #-}
-- >
-- > module Main
-- > ( main
-- > ) where
-- >
-- > import Configuration.Utils
-- > import PkgInfo
-- >
-- > instance FromJSON (() -> ()) where parseJSON _ = pure id
-- >
-- > mainInfo :: ProgramInfo ()
-- > mainInfo = programInfo "Hello World" (pure id) ()
-- >
-- > main :: IO ()
-- > main = runWithPkgInfoConfiguration mainInfo pkgInfo . const $ putStrLn "hello world"
--
-- With that the resulting application supports the following additional command
-- line options:
--
-- [@--version@, @-v@]
--     prints the version of the application and exits.
--
-- [@--info@, @-i@]
--     prints a short info message for the application and exits.
--
-- [@--long-info@]
--     print a detailed info message for the application and exits.
--     Beside component name, package name, version, revision, and copyright
--     the message also contain information about the compiler that
--     was used for the build, the build architecture, build flags,
--     the author, the license type, and a list of all direct and
--     indirect dependencies along with their licenses and copyrights.
--
-- [@--license@]
--     prints the text of the lincense of the application and exits.
--
module Main
( main
) where

import qualified Distribution.Compat.Graph as Graph
import qualified Distribution.InstalledPackageInfo as I
import Distribution.PackageDescription
import Distribution.Pretty
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Simple.Setup
import Distribution.Text
import Distribution.Types.LocalBuildInfo
import Distribution.Types.UnqualComponentName
import Distribution.Utils.Path
import Distribution.Utils.ShortText

import System.Process

import Control.Applicative
import Control.Monad

import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid

import System.Directory
    (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist,
    doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath (isDrive, takeDirectory)

-- | Include this function when your setup doesn't contain any
-- extra functionality.
--
main :: IO ()
main = defaultMainWithHooks (mkPkgInfoModules simpleUserHooks)

-- | Modifies the given record of hooks by adding functionality that
-- creates a package info module for each component of the cabal package.
--
-- This function is intended for usage in more complex @Setup.hs@ scripts.
-- If your setup doesn't contain any other function you can just import
-- the 'main' function from this module.
--
-- The modules are created in the /autogen/ build directories where also the
-- @Path_@ modules are created by cabal's simple build setup.
--
mkPkgInfoModules
    :: UserHooks
    -> UserHooks
mkPkgInfoModules hooks = hooks
    { postConf = mkPkgInfoModulesPostConf (postConf hooks)
    }

-- -------------------------------------------------------------------------- --
-- Compat Implementations

prettyLicense :: I.InstalledPackageInfo -> String
prettyLicense = either prettyShow prettyShow . I.license

ft :: ShortText -> String
ft = fromShortText

-- -------------------------------------------------------------------------- --
-- Cabal 2.0

mkPkgInfoModulesPostConf
    :: (Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ())
    -> Args
    -> ConfigFlags
    -> PackageDescription
    -> LocalBuildInfo
    -> IO ()
mkPkgInfoModulesPostConf hook args flags pkgDesc bInfo = do
    mapM_ (updatePkgInfoModule pkgDesc bInfo) $ Graph.toList $ componentGraph bInfo
    hook args flags pkgDesc bInfo

updatePkgInfoModule :: PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> IO ()
updatePkgInfoModule pkgDesc bInfo clbInfo = do
    createDirectoryIfMissing True $ interpretSymbolicPathCWD dirName
    moduleBytes <- pkgInfoModule moduleName cName pkgDesc bInfo
    updateFile fileName moduleBytes

    -- legacy module
    legacyModuleBytes <- pkgInfoModule legacyModuleName cName pkgDesc bInfo
    updateFile legacyFileName legacyModuleBytes

  where
    dirName = autogenComponentModulesDir bInfo clbInfo
    cName = unUnqualComponentName <$> componentNameString (componentLocalName clbInfo)

    moduleName = pkgInfoModuleName
    fileName = dirName </> unsafeMakeSymbolicPath moduleName <.> ".hs"

    legacyModuleName = legacyPkgInfoModuleName cName
    legacyFileName = dirName </> unsafeMakeSymbolicPath legacyModuleName <.> ".hs"

-- -------------------------------------------------------------------------- --
-- Generate PkgInfo Module

pkgInfoModuleName :: String
pkgInfoModuleName = "PkgInfo"

updateFile :: SymbolicPath from to -> B.ByteString -> IO ()
updateFile (interpretSymbolicPathCWD -> fileName) content = do
    x <- doesFileExist fileName
    if | not x -> update
       | otherwise -> do
           oldRevisionFile <- B.readFile fileName
           when (oldRevisionFile /= content) update
  where
    update = B.writeFile fileName content

legacyPkgInfoModuleName :: Maybe String -> String
legacyPkgInfoModuleName Nothing = "PkgInfo"
legacyPkgInfoModuleName (Just cn) = "PkgInfo_" ++ map tr cn
  where
    tr '-' = '_'
    tr c = c

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- -------------------------------------------------------------------------- --
-- VCS

getVCS :: IO (Maybe KnownRepoType)
getVCS = getCurrentDirectory >>= getVcsOfDir
  where
    getVcsOfDir d = do
        canonicDir <- canonicalizePath d
        doesDirectoryExist (canonicDir </> ".hg") >>= \x0 -> if x0
        then return (Just Mercurial)
        else doesDirectoryExist (canonicDir </> ".git") >>= \x1 -> if x1
            then return $ Just Git
            else if isDrive canonicDir
                then return Nothing
                else getVcsOfDir (takeDirectory canonicDir)

-- | Returns tag, revision, and branch name.
--
hgInfo :: IO (String, String, String)
hgInfo = do
    tag <- trim <$> readProcess "hg" ["id", "-r", "max(ancestors(\".\") and tag())", "-t"] ""
    rev <- trim <$> readProcess "hg" ["id", "-i"] ""
    branch <- trim <$> readProcess "hg" ["id", "-b"] ""
    return (tag, rev, branch)

-- | Returns tag, revision, and branch name.
--
gitInfo :: IO (String, String, String)
gitInfo = do
    tag <- do
        (exitCode, out, _err) <- readProcessWithExitCode "git" ["describe", "--exact-match", "--tags", "--abbrev=0"] ""
        case exitCode of
            ExitSuccess -> return $ trim out
            _ -> return ""
    rev <- trim <$> readProcess "git" ["rev-parse", "--short", "HEAD"] ""
    branch <- trim <$> readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""
    return (tag, rev, branch)

-- | Returns tag, revision, and branch name.
--
gitlabCiVcsInfo :: IO (Maybe (String, String, String))
gitlabCiVcsInfo = do
    lookupEnv "CI_COMMIT_SHORT_SHA" >>= \case
        Nothing -> return Nothing
        Just _ -> do
            rev <- fromMaybe "" <$> lookupEnv "CI_COMMIT_SHORT_SHA"
            branch <- fromMaybe "" <$> lookupEnv "CI_COMMIT_REF_NAME"
            tag <- fromMaybe "" <$> lookupEnv "CI_COMMIT_TAG"
            return $ Just (tag, rev, branch)

-- | The file format is revision, branch, and tag separate by newline
-- characters.
--
fileVcsInfo :: IO (Maybe (String, String, String))
fileVcsInfo = do
    doesFileExist ".vcs-info" >>= \x -> if x
    then do
        (rev : branch : tag : _) <- (<> ["", "", ""]) . lines <$> readFile ".vcs-info"
        return $ Just (tag, rev, branch)
    else return Nothing

-- | Returns tag, revision, and branch name.
--
noVcsInfo :: IO (String, String, String)
noVcsInfo = return ("", "", "")

-- | Returns tag, revision, and branch name.
--
getVcsInfo :: IO (String, String, String)
getVcsInfo = getVCS >>= \case
    Just Mercurial -> hgInfo
    Just Git -> gitInfo
    _ -> gitlabCiVcsInfo >>= \case
        Just a -> return a
        Nothing -> fileVcsInfo >>= \case
            Just a -> return a
            Nothing -> noVcsInfo

-- -------------------------------------------------------------------------- --
-- Generate Module

pkgInfoModule :: String -> Maybe String -> PackageDescription -> LocalBuildInfo -> IO B.ByteString
pkgInfoModule moduleName cName pkgDesc bInfo = do
    (tag, revision, branch) <- getVcsInfo

    let vcsBranch = if branch == "default" || branch == "master" then "" else branch
        vcsVersion = intercalate "-" . filter (/= "") $ [tag, revision, vcsBranch]
        flags = map (unFlagName . fst) . filter snd . unFlagAssignment . configConfigurationsFlags . configFlags $ bInfo

    licenseString <- licenseFilesText pkgDesc

    return $ B.intercalate "\n"
            [ "{-# LANGUAGE OverloadedStrings #-}"
            , "{-# LANGUAGE RankNTypes #-}"
            , ""
            , "module " <> pack moduleName <> " " <> deprecatedMsg <> " where"
            , ""
            , "    import Data.String (IsString)"
            , "    import Data.Monoid"
            , "    import Prelude hiding ((<>))"
            , ""
            , "    name :: IsString a => Maybe a"
            , "    name = " <> maybe "Nothing" (\x -> "Just \"" <> pack x <> "\"") cName
            , ""
            , "    tag :: IsString a => a"
            , "    tag = \"" <> pack tag <> "\""
            , ""
            , "    revision :: IsString a => a"
            , "    revision = \"" <> pack revision <> "\""
            , ""
            , "    branch :: IsString a => a"
            , "    branch = \"" <> pack branch <> "\""
            , ""
            , "    branch' :: IsString a => a"
            , "    branch' = \"" <> pack vcsBranch <> "\""
            , ""
            , "    vcsVersion :: IsString a => a"
            , "    vcsVersion = \"" <> pack vcsVersion <> "\""
            , ""
            , "    compiler :: IsString a => a"
            , "    compiler = \"" <> (pack . display . compilerId . compiler) bInfo <> "\""
            , ""
            , "    flags :: IsString a => [a]"
            , "    flags = " <> (pack . show) flags
            , ""
            , "    optimisation :: IsString a => a"
            , "    optimisation = \"" <> (displayOptimisationLevel . withOptimization) bInfo <> "\""
            , ""
            , "    arch :: IsString a => a"
            , "    arch = \"" <> (pack . display . hostPlatform) bInfo <> "\""
            , ""
            , "    license :: IsString a => a"
            , "    license = \"" <> (pack . prettyShow . license) pkgDesc <> "\""
            , ""
            , "    licenseText :: IsString a => a"
            , "    licenseText = " <> (pack . show) licenseString
            , ""
            , "    copyright :: IsString a => a"
            , "    copyright = " <> (pack . show . copyright) pkgDesc
            , ""
            , "    author :: IsString a => a"
            , "    author = \"" <> (pack . ft . author) pkgDesc <> "\""
            , ""
            , "    homepage :: IsString a => a"
            , "    homepage = \"" <> (pack . ft . homepage) pkgDesc <> "\""
            , ""
            , "    package :: IsString a => a"
            , "    package = \"" <> (pack . display . package) pkgDesc <> "\""
            , ""
            , "    packageName :: IsString a => a"
            , "    packageName = \"" <> (pack . display . packageName) pkgDesc <> "\""
            , ""
            , "    packageVersion :: IsString a => a"
            , "    packageVersion = \"" <> (pack . display . packageVersion) pkgDesc <> "\""
            , ""
            , "    dependencies :: IsString a => [a]"
            , "    dependencies = " <> (pack . show . map (display . packageId) . allPackages . installedPkgs) bInfo
            , ""
            , "    dependenciesWithLicenses :: IsString a => [a]"
            , "    dependenciesWithLicenses = " <> (pack . show . map pkgIdWithLicense . allPackages . installedPkgs) bInfo
            , ""
            , "    versionString :: (Monoid a, IsString a) => a"
            , "    versionString = case name of"
            , "        Nothing -> package <> \" (revision \" <> vcsVersion <> \")\""
            , "        Just n -> n <> \"-\" <> packageVersion <> \" (package \" <> package <> \" revision \" <> vcsVersion <> \")\""
            , ""
            , "    info :: (Monoid a, IsString a) => a"
            , "    info = versionString <> \"\\n\" <> copyright"
            , ""
            , "    longInfo :: (Monoid a, IsString a) => a"
            , "    longInfo = info <> \"\\n\\n\""
            , "        <> \"Author: \" <> author <> \"\\n\""
            , "        <> \"License: \" <> license <> \"\\n\""
            , "        <> \"Homepage: \" <> homepage <> \"\\n\""
            , "        <> \"Build with: \" <> compiler <> \" (\" <> arch <> \")\" <> \"\\n\""
            , "        <> \"Build flags: \" <> mconcat (map (\\x -> \" \" <> x) flags) <> \"\\n\""
            , "        <> \"Optimisation: \" <> optimisation <> \"\\n\\n\""
            , "        <> \"Dependencies:\\n\" <> mconcat (map (\\x -> \"    \" <> x <> \"\\n\") dependenciesWithLicenses)"
            , ""
            , "    pkgInfo :: (Monoid a, IsString a) => (a, a, a, a)"
            , "    pkgInfo ="
            , "        ( info"
            , "        , longInfo"
            , "        , versionString"
            , "        , licenseText"
            , "        )"
            , ""
            ]
  where
    displayOptimisationLevel NoOptimisation = "none"
    displayOptimisationLevel NormalOptimisation = "normal"
    displayOptimisationLevel MaximumOptimisation = "maximum"

    deprecatedMsg = if moduleName /= pkgInfoModuleName
        then "{-# DEPRECATED \"Update to Cabal 2.0 or later and use just PkgInfo as module name.\" #-}"
        else ""

licenseFilesText :: PackageDescription -> IO B.ByteString
licenseFilesText pkgDesc =
    B.intercalate "\n------------------------------------------------------------\n" <$> mapM fileTextStr
        (licenseFiles pkgDesc)
  where
    fileTextStr = fileText . getSymbolicPath
    fileText file = doesFileExist file >>= \x -> if x
        then B.readFile file
        else return ""

pkgIdWithLicense :: I.InstalledPackageInfo -> String
pkgIdWithLicense a = (display . packageId) a
    ++ " ["
    ++ prettyLicense a
    ++ (if cr /= "" then ", " ++ cr else "")
    ++ "]"
  where
    cr = (unwords . words . ft . I.copyright) a
