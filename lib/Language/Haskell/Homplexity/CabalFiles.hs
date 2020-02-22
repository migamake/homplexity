{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Haskell.Homplexity.CabalFiles
    ( CabalFile
    , CabalPackage(..)
    , warnings
    , parseCabalFile
    , languageExtensions
    ) where


import Data.Generics.Uniplate.Data as U
import Data.String (fromString)
import Language.Haskell.Extension as Cabal
import Language.Haskell.Exts.Extension as HSE
import Language.Haskell.Exts.SrcLoc
import qualified Data.ByteString as BS

import Language.Haskell.Homplexity.Message

#if MIN_VERSION_Cabal(3,0,0)
import Distribution.PackageDescription.Parsec
import Distribution.Parsec.Warning
import Distribution.Types.GenericPackageDescription
#elif MIN_VERSION_Cabal(2,0,0)
import Distribution.PackageDescription.Parsec
import Distribution.Types.GenericPackageDescription
import Distribution.Parsec.Common
#else
import Distribution.PackageDescription(GenericPackageDescription(..))
import Distribution.PackageDescription.Parse(parsePackageDescription)
import Distribution.ParseUtils

parseGenericPackageDescription = parsePackageDescription . BS.unpack
#endif



-- | Project file cabal file related info
--
data CabalFile = CabalFile
    { warnings    :: [PWarning]
    , description :: GenericPackageDescription
    }
    deriving (Show)


parseCabalFile :: FilePath -> IO (Either Log CabalFile)
parseCabalFile cabalFilePath = do
    cabalFile <- BS.readFile cabalFilePath
    case runParseResult $ parseGenericPackageDescription cabalFile of
        (_, Left err) ->
            return $ Left $ critical (SrcLoc cabalFilePath 0 0) (show err)
        (warns, Right desc) ->
            return $ Right $ CabalFile warns desc


cabalExtensionToHseExtension :: Cabal.Extension -> HSE.Extension
cabalExtensionToHseExtension (Cabal.UnknownExtension ex) = HSE.UnknownExtension ex
cabalExtensionToHseExtension (Cabal.EnableExtension ex)  = HSE.EnableExtension (cabalKnownExtensionToHseKnownExtension ex)
cabalExtensionToHseExtension (Cabal.DisableExtension ex) = HSE.DisableExtension (cabalKnownExtensionToHseKnownExtension ex)


cabalKnownExtensionToHseKnownExtension :: Cabal.KnownExtension -> HSE.KnownExtension
cabalKnownExtensionToHseKnownExtension Cabal.OverlappingInstances = HSE.OverlappingInstances
-- cabalKnownExtensionToHseKnownExtension Cabal.AllowAmbiguousTypes = HSE.AllowAmbiguousTypes
-- cabalKnownExtensionToHseKnownExtension Cabal.ApplicativeDo = HSE.ApplicativeDo
cabalKnownExtensionToHseKnownExtension Cabal.Arrows = HSE.Arrows
-- cabalKnownExtensionToHseKnownExtension Cabal.AutoDeriveTypeable = HSE.AutoDeriveTypeable
cabalKnownExtensionToHseKnownExtension Cabal.BangPatterns = HSE.BangPatterns
cabalKnownExtensionToHseKnownExtension Cabal.BinaryLiterals = HSE.BinaryLiterals
-- cabalKnownExtensionToHseKnownExtension Cabal.BlockArguments = HSE.BlockArguments
cabalKnownExtensionToHseKnownExtension Cabal.CApiFFI = HSE.CApiFFI
cabalKnownExtensionToHseKnownExtension Cabal.CPP = HSE.CPP
cabalKnownExtensionToHseKnownExtension Cabal.ConstrainedClassMethods = HSE.ConstrainedClassMethods
cabalKnownExtensionToHseKnownExtension Cabal.ConstraintKinds = HSE.ConstraintKinds
cabalKnownExtensionToHseKnownExtension Cabal.DataKinds = HSE.DataKinds
cabalKnownExtensionToHseKnownExtension Cabal.DatatypeContexts = HSE.DatatypeContexts
cabalKnownExtensionToHseKnownExtension Cabal.DefaultSignatures = HSE.DefaultSignatures
cabalKnownExtensionToHseKnownExtension Cabal.DeriveDataTypeable = HSE.DeriveDataTypeable
cabalKnownExtensionToHseKnownExtension Cabal.DeriveFoldable = HSE.DeriveFoldable
cabalKnownExtensionToHseKnownExtension Cabal.DeriveFunctor = HSE.DeriveFunctor
cabalKnownExtensionToHseKnownExtension Cabal.DeriveGeneric = HSE.DeriveGeneric
-- cabalKnownExtensionToHseKnownExtension Cabal.DeriveLift = HSE.DeriveLift
cabalKnownExtensionToHseKnownExtension Cabal.DeriveTraversable = HSE.DeriveTraversable
cabalKnownExtensionToHseKnownExtension Cabal.DisambiguateRecordFields = HSE.DisambiguateRecordFields
cabalKnownExtensionToHseKnownExtension Cabal.DoAndIfThenElse = HSE.DoAndIfThenElse
cabalKnownExtensionToHseKnownExtension Cabal.DoRec = HSE.DoRec
-- cabalKnownExtensionToHseKnownExtension Cabal.DuplicateRecordFields = HSE.DuplicateRecordFields
cabalKnownExtensionToHseKnownExtension Cabal.EmptyDataDecls = HSE.EmptyDataDecls
cabalKnownExtensionToHseKnownExtension Cabal.ExistentialQuantification = HSE.ExistentialQuantification
cabalKnownExtensionToHseKnownExtension Cabal.ExplicitForAll = HSE.ExplicitForAll
cabalKnownExtensionToHseKnownExtension Cabal.ExplicitNamespaces = HSE.ExplicitNamespaces
cabalKnownExtensionToHseKnownExtension Cabal.ExtendedDefaultRules = HSE.ExtendedDefaultRules
cabalKnownExtensionToHseKnownExtension Cabal.ExtensibleRecords = HSE.ExtensibleRecords
cabalKnownExtensionToHseKnownExtension Cabal.FlexibleContexts = HSE.FlexibleContexts
cabalKnownExtensionToHseKnownExtension Cabal.FlexibleInstances = HSE.FlexibleInstances
cabalKnownExtensionToHseKnownExtension Cabal.ForeignFunctionInterface = HSE.ForeignFunctionInterface
cabalKnownExtensionToHseKnownExtension Cabal.FunctionalDependencies = HSE.FunctionalDependencies
-- cabalKnownExtensionToHseKnownExtension Cabal.GADTSyntax = HSE.GADTSyntax
cabalKnownExtensionToHseKnownExtension Cabal.GADTs = HSE.GADTs
cabalKnownExtensionToHseKnownExtension Cabal.GHCForeignImportPrim = HSE.GHCForeignImportPrim
cabalKnownExtensionToHseKnownExtension Cabal.GeneralizedNewtypeDeriving = HSE.GeneralizedNewtypeDeriving
cabalKnownExtensionToHseKnownExtension Cabal.Generics = HSE.Generics
cabalKnownExtensionToHseKnownExtension Cabal.HereDocuments = HSE.HereDocuments
-- cabalKnownExtensionToHseKnownExtension Cabal.HexFloatLiterals = HSE.HexFloatLiterals
cabalKnownExtensionToHseKnownExtension Cabal.ImplicitParams = HSE.ImplicitParams
cabalKnownExtensionToHseKnownExtension Cabal.ImplicitPrelude = HSE.ImplicitPrelude
cabalKnownExtensionToHseKnownExtension Cabal.ImpredicativeTypes = HSE.ImpredicativeTypes
cabalKnownExtensionToHseKnownExtension Cabal.IncoherentInstances = HSE.IncoherentInstances
cabalKnownExtensionToHseKnownExtension Cabal.InstanceSigs = HSE.InstanceSigs
cabalKnownExtensionToHseKnownExtension Cabal.InterruptibleFFI = HSE.InterruptibleFFI
cabalKnownExtensionToHseKnownExtension Cabal.KindSignatures = HSE.KindSignatures
cabalKnownExtensionToHseKnownExtension Cabal.LambdaCase = HSE.LambdaCase
cabalKnownExtensionToHseKnownExtension Cabal.LiberalTypeSynonyms = HSE.LiberalTypeSynonyms
cabalKnownExtensionToHseKnownExtension Cabal.MagicHash = HSE.MagicHash
-- cabalKnownExtensionToHseKnownExtension Cabal.MonadComprehensions = HSE.MonadComprehensions
-- cabalKnownExtensionToHseKnownExtension Cabal.MonadFailDesugaring = HSE.MonadFailDesugaring
cabalKnownExtensionToHseKnownExtension Cabal.MonoLocalBinds = HSE.MonoLocalBinds
cabalKnownExtensionToHseKnownExtension Cabal.MonoPatBinds = HSE.MonoPatBinds
cabalKnownExtensionToHseKnownExtension Cabal.MonomorphismRestriction = HSE.MonomorphismRestriction
cabalKnownExtensionToHseKnownExtension Cabal.MultiParamTypeClasses = HSE.MultiParamTypeClasses
cabalKnownExtensionToHseKnownExtension Cabal.MultiWayIf = HSE.MultiWayIf
cabalKnownExtensionToHseKnownExtension Cabal.NPlusKPatterns = HSE.NPlusKPatterns
cabalKnownExtensionToHseKnownExtension Cabal.NamedFieldPuns = HSE.NamedFieldPuns
cabalKnownExtensionToHseKnownExtension Cabal.NamedWildCards = HSE.NamedWildCards
-- cabalKnownExtensionToHseKnownExtension Cabal.NegativeLiterals = HSE.NegativeLiterals
cabalKnownExtensionToHseKnownExtension Cabal.NewQualifiedOperators = HSE.NewQualifiedOperators
cabalKnownExtensionToHseKnownExtension Cabal.NondecreasingIndentation = HSE.NondecreasingIndentation
-- cabalKnownExtensionToHseKnownExtension Cabal.NullaryTypeClasses = HSE.NullaryTypeClasses
-- cabalKnownExtensionToHseKnownExtension Cabal.NumDecimals = HSE.NumDecimals
-- cabalKnownExtensionToHseKnownExtension Cabal.NumericUnderscores = HSE.NumericUnderscores
-- cabalKnownExtensionToHseKnownExtension Cabal.OverlappingInstances = HSE.OverlappingInstances
cabalKnownExtensionToHseKnownExtension Cabal.OverloadedLabels = HSE.OverloadedLabels
-- cabalKnownExtensionToHseKnownExtension Cabal.OverloadedLists = HSE.OverloadedLists
cabalKnownExtensionToHseKnownExtension Cabal.OverloadedStrings = HSE.OverloadedStrings
cabalKnownExtensionToHseKnownExtension Cabal.PackageImports = HSE.PackageImports
cabalKnownExtensionToHseKnownExtension Cabal.ParallelArrays = HSE.ParallelArrays
cabalKnownExtensionToHseKnownExtension Cabal.ParallelListComp = HSE.ParallelListComp
cabalKnownExtensionToHseKnownExtension Cabal.PartialTypeSignatures = HSE.PartialTypeSignatures
cabalKnownExtensionToHseKnownExtension Cabal.PatternGuards = HSE.PatternGuards
cabalKnownExtensionToHseKnownExtension Cabal.PatternSignatures = HSE.PatternSignatures
cabalKnownExtensionToHseKnownExtension Cabal.PatternSynonyms = HSE.PatternSynonyms
cabalKnownExtensionToHseKnownExtension Cabal.PolyKinds = HSE.PolyKinds
cabalKnownExtensionToHseKnownExtension Cabal.PolymorphicComponents = HSE.PolymorphicComponents
cabalKnownExtensionToHseKnownExtension Cabal.PostfixOperators = HSE.PostfixOperators
-- cabalKnownExtensionToHseKnownExtension Cabal.QuantifiedConstraints = HSE.QuantifiedConstraints
cabalKnownExtensionToHseKnownExtension Cabal.QuasiQuotes = HSE.QuasiQuotes
cabalKnownExtensionToHseKnownExtension Cabal.Rank2Types = HSE.Rank2Types
cabalKnownExtensionToHseKnownExtension Cabal.RankNTypes = HSE.RankNTypes
cabalKnownExtensionToHseKnownExtension Cabal.RebindableSyntax = HSE.RebindableSyntax
cabalKnownExtensionToHseKnownExtension Cabal.RecordPuns = HSE.RecordPuns
cabalKnownExtensionToHseKnownExtension Cabal.RecordWildCards = HSE.RecordWildCards
cabalKnownExtensionToHseKnownExtension Cabal.RecursiveDo = HSE.RecursiveDo
cabalKnownExtensionToHseKnownExtension Cabal.RegularPatterns = HSE.RegularPatterns
cabalKnownExtensionToHseKnownExtension Cabal.RelaxedPolyRec = HSE.RelaxedPolyRec
cabalKnownExtensionToHseKnownExtension Cabal.RestrictedTypeSynonyms = HSE.RestrictedTypeSynonyms
cabalKnownExtensionToHseKnownExtension Cabal.RoleAnnotations = HSE.RoleAnnotations
cabalKnownExtensionToHseKnownExtension Cabal.Safe = HSE.Safe
cabalKnownExtensionToHseKnownExtension Cabal.SafeImports = HSE.SafeImports
cabalKnownExtensionToHseKnownExtension Cabal.ScopedTypeVariables = HSE.ScopedTypeVariables
cabalKnownExtensionToHseKnownExtension Cabal.StandaloneDeriving = HSE.StandaloneDeriving
-- cabalKnownExtensionToHseKnownExtension Cabal.StarIsType = HSE.StarIsType
-- cabalKnownExtensionToHseKnownExtension Cabal.StaticPointers = HSE.StaticPointers
-- cabalKnownExtensionToHseKnownExtension Cabal.Strict = HSE.Strict
-- cabalKnownExtensionToHseKnownExtension Cabal.StrictData = HSE.StrictData
cabalKnownExtensionToHseKnownExtension Cabal.TemplateHaskell = HSE.TemplateHaskell
-- cabalKnownExtensionToHseKnownExtension Cabal.TemplateHaskellQuotes = HSE.TemplateHaskellQuotes
-- cabalKnownExtensionToHseKnownExtension Cabal.TraditionalRecordSyntax = HSE.TraditionalRecordSyntax
cabalKnownExtensionToHseKnownExtension Cabal.TransformListComp = HSE.TransformListComp
cabalKnownExtensionToHseKnownExtension Cabal.Trustworthy = HSE.Trustworthy
cabalKnownExtensionToHseKnownExtension Cabal.TupleSections = HSE.TupleSections
cabalKnownExtensionToHseKnownExtension Cabal.TypeApplications = HSE.TypeApplications
cabalKnownExtensionToHseKnownExtension Cabal.TypeFamilies = HSE.TypeFamilies
-- cabalKnownExtensionToHseKnownExtension Cabal.TypeInType = HSE.TypeInType
cabalKnownExtensionToHseKnownExtension Cabal.TypeOperators = HSE.TypeOperators
cabalKnownExtensionToHseKnownExtension Cabal.TypeSynonymInstances = HSE.TypeSynonymInstances
cabalKnownExtensionToHseKnownExtension Cabal.UnboxedTuples = HSE.UnboxedTuples
cabalKnownExtensionToHseKnownExtension Cabal.UndecidableInstances = HSE.UndecidableInstances
-- cabalKnownExtensionToHseKnownExtension Cabal.UndecidableSuperClasses = HSE.UndecidableSuperClasses
cabalKnownExtensionToHseKnownExtension Cabal.UnicodeSyntax = HSE.UnicodeSyntax
cabalKnownExtensionToHseKnownExtension Cabal.UnliftedFFITypes = HSE.UnliftedFFITypes
-- cabalKnownExtensionToHseKnownExtension Cabal.Unsafe = HSE.Unsafe
cabalKnownExtensionToHseKnownExtension Cabal.ViewPatterns = HSE.ViewPatterns
cabalKnownExtensionToHseKnownExtension Cabal.XmlSyntax = HSE.XmlSyntax
cabalKnownExtensionToHseKnownExtension Cabal.DeriveAnyClass = HSE.DeriveAnyClass
cabalKnownExtensionToHseKnownExtension Cabal.DerivingStrategies = HSE.DerivingStrategies
cabalKnownExtensionToHseKnownExtension Cabal.EmptyCase = HSE.EmptyCase
cabalKnownExtensionToHseKnownExtension Cabal.JavaScriptFFI = HSE.JavaScriptFFI
cabalKnownExtensionToHseKnownExtension Cabal.TypeFamilyDependencies = HSE.TypeFamilyDependencies
cabalKnownExtensionToHseKnownExtension Cabal.UnboxedSums = HSE.UnboxedSums

cabalKnownExtensionToHseKnownExtension ex = error $ "Extension '" ++ show ex ++ "' unsupported"


-- | Cabal package identification
--
data CabalPackage = Library | Package String


languageExtensions :: CabalPackage -> CabalFile -> [HSE.Extension]
languageExtensions moduleName CabalFile{description} = map cabalExtensionToHseExtension $ languageExtensions' moduleName
  where
    languageExtensions' Library = [e | (e :: Cabal.Extension) <- U.universeBi (condLibrary description)]
    languageExtensions' (Package pname) =
        let pname' = fromString pname
            filt = filter (\(name,_) -> name == pname')
        in [e | (e :: Cabal.Extension) <- U.universeBi (filt $ condForeignLibs description)] ++
           [e | (e :: Cabal.Extension) <- U.universeBi (filt $ condExecutables description)] ++
           [e | (e :: Cabal.Extension) <- U.universeBi (filt $ condTestSuites description) ] ++
           [e | (e :: Cabal.Extension) <- U.universeBi (filt $ condBenchmarks description) ]

