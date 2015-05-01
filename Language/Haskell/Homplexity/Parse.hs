{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Parsing of Haskell source files, and error reporting for unparsable files.
module Language.Haskell.Homplexity.Parse (parseSource) where

import Data.Char
import Data.Data
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Control.Arrow
import Control.Exception as E
import Control.Monad

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts
import Language.Haskell.Homplexity.Cyclomatic
import Language.Haskell.Homplexity.Metric
import Language.Haskell.Homplexity.CodeFragment
import Language.Haskell.Homplexity.Comments
import Language.Haskell.Homplexity.Message
import Language.Preprocessor.Cpphs
import System.Directory
import System.Environment
import System.FilePath
import System.IO

import HFlags

-- | Maximally permissive list of language extensions.
myExtensions ::  [Extension]
myExtensions = map EnableExtension
               [RecordWildCards,
                ScopedTypeVariables, CPP, MultiParamTypeClasses, TemplateHaskell,  RankNTypes, UndecidableInstances,
                FlexibleContexts, KindSignatures, EmptyDataDecls, BangPatterns, ForeignFunctionInterface,
                Generics, MagicHash, ViewPatterns, PatternGuards, TypeOperators, GADTs, PackageImports,
                MultiWayIf, SafeImports, ConstraintKinds, TypeFamilies, IncoherentInstances, FunctionalDependencies,
                ExistentialQuantification, ImplicitParams, UnicodeSyntax]

-- | CppHs options that should be compatible with haskell-src-exts
cppHsOptions ::  CpphsOptions
cppHsOptions = defaultCpphsOptions {
                 boolopts = defaultBoolOptions {
                              macros    = False,
                              stripEol  = True,
                              stripC89  = True,
                              pragma    = False,
                              hashline  = False,
                              locations = True -- or False if doesn't compile...
                            }
               }

-- | Parse Haskell source file, using CppHs for preprocessing,
-- and haskell-src-exts for parsing.
--
-- Catches all exceptions and wraps them as @Critical@ log messages.
parseSource ::  FilePath -> IO (Either Log (Module, [CommentLink]))
parseSource filename = do
  parsed <- (do
    --putStrLn $ "\nProcessing " ++ filename ++ ":"
    input   <- readFile filename
    result  <- parseModuleWithComments parseMode <$> runCpphs cppHsOptions filename input
    evaluate result)
      `E.catch` handleException (ParseFailed thisFileLoc)
  case parsed of
    ParseOk (parsed, comments) ->    --putStrLn $ unlines $ map show $ classifyComments comments
                                     return $ Right (parsed, classifyComments comments)
    ParseFailed loc msg        ->    return $ Left $ critical loc msg
  where
    handleException helper (e :: SomeException) = return $ helper $ show e
    thisFileLoc = noLoc { srcFilename = filename }
    parseMode = ParseMode {
                  parseFilename         = filename,
                  baseLanguage          = Haskell2010,
                  extensions            = myExtensions,
                  ignoreLanguagePragmas = False,
                  ignoreLinePragmas     = False,
                  fixities              = Just preludeFixities
                }

