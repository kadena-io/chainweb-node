{-# language
    ImportQualifiedPost
  , OverloadedRecordDot
  , PackageImports
#-}

module Utils.PackageInfo
  ( pkgInfo
  , revisionExp
  , compiler
  , arch
  , os
  )
  where

import "Cabal-syntax" Distribution.Pretty qualified as Cabal (prettyShow)
import "Cabal-syntax" Distribution.Types.GenericPackageDescription (GenericPackageDescription(..))
import "Cabal-syntax" Distribution.Types.PackageDescription (PackageDescription(..))
import "Cabal-syntax" Distribution.Types.PackageId (PackageIdentifier(..))
import "Cabal-syntax" Distribution.Utils.ShortText qualified as CabalTextShort
import "base" Control.Applicative ((<|>))
import "base" Control.Monad.IO.Class (liftIO)
import "base" Data.Char (isSpace)
import "base" Data.List qualified as List
import "base" Data.Version qualified as BaseVersion
import "base" System.Info qualified
import "cabal-install-parsers" Cabal.Package qualified as Cabal
import "configuration-tools" Configuration.Utils qualified as ConfigTools
import "process" System.Process (readProcess)
import "template-haskell" Language.Haskell.TH qualified as TH
import "template-haskell" Language.Haskell.TH.Syntax qualified as TH

revisionExp :: TH.Code TH.Q String
revisionExp = TH.liftCode $ fmap (TH.TExp . TH.LitE . TH.stringL) revisionQ

compiler :: String
compiler = System.Info.compilerName <> "-" <> BaseVersion.showVersion System.Info.fullCompilerVersion

arch :: String
arch = System.Info.arch

os :: String
os = System.Info.os

-- Used internally
revisionQ :: TH.Q String
revisionQ = liftIO $ do
  gitShortRev <- do
    let get = trim <$> readProcess "git" ["rev-parse", "--short", "HEAD"] ""
    get <|> pure "<unknown package git revision>"
  pure gitShortRev

pkgInfo :: TH.Code TH.Q ConfigTools.PkgInfo
pkgInfo =
  TH.bindCode revisionQ $ \revision ->
    TH.liftCode $ liftIO $ do
      cabalDesc <- do
        packageDescription <$> Cabal.readPackage "chainweb.cabal"

      license <- do
        let get = do
              trim <$> readFile "LICENSE"
        get <|> pure "<unknown package license>"

      dependencyInfo <- do
        let get = do
              trim <$> readProcess "cabal-plan" ["license-report", "--licensedir", ".", "exe:chainweb-node"] ""
        get <|> pure "<unknown package dependencies>"

      let packageVersion = Cabal.prettyShow cabalDesc.package.pkgVersion
      let packageCopyright = CabalTextShort.fromShortText cabalDesc.copyright

      let version = "chainweb-node " <> packageVersion <> " (revision " <> revision <> ")"
      let info = List.intercalate "\n" [version, packageCopyright]

      let longInfo = List.intercalate "\n"
            [ version
            , packageCopyright
            , ""
            , "Author: " <> CabalTextShort.fromShortText cabalDesc.author
            , "License: " <> either Cabal.prettyShow Cabal.prettyShow cabalDesc.licenseRaw
            , "Homepage: " <> CabalTextShort.fromShortText cabalDesc.homepage
            , "Compiler: " <> compiler <> " (" <> arch <> "-" <> os <> ")"
            , ""
            , ""
            ] <> dependencyInfo

      pure $ TH.TExp $ TH.TupE $ List.map (Just . TH.LitE . TH.stringL) [info, longInfo, version, license]

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace