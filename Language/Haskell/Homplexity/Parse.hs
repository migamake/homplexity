{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Parsing of Haskell source files, and error reporting for unparsable files.
module Language.Haskell.Homplexity.Parse (parseSource) where

import Control.Exception as E
import Data.Functor

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts
import Language.Haskell.Homplexity.Comments
import Language.Haskell.Homplexity.Message
import Language.Preprocessor.Cpphs

--import HFlags

-- | Maximally permissive list of language extensions.
myExtensions ::  [Extension]
myExtensions = EnableExtension `map`
               [RecordWildCards,
                ScopedTypeVariables, CPP, MultiParamTypeClasses, TemplateHaskell,  RankNTypes, UndecidableInstances,
                FlexibleContexts, KindSignatures, EmptyDataDecls, BangPatterns, ForeignFunctionInterface,
                Generics, MagicHash, ViewPatterns, PatternGuards, TypeOperators, GADTs, PackageImports,
                MultiWayIf, SafeImports, ConstraintKinds, TypeFamilies, IncoherentInstances, FunctionalDependencies,
                ExistentialQuantification, ImplicitParams, UnicodeSyntax,
                LambdaCase, TupleSections, NamedFieldPuns]

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
parseSource ::  FilePath -> IO (Either Log (Module SrcLoc, [CommentLink]))
parseSource inputFilename = do
  parseResult <- (do
    input   <- readFile inputFilename
    result  <- parseModuleWithComments parseMode <$> runCpphs cppHsOptions inputFilename input
    evaluate result)
      `E.catch` handleException (ParseFailed thisFileLoc)
  case parseResult of
    ParseOk (parsed, comments) -> do putStrLn   "ORDERED:"
                                     putStrLn $ unlines $ map show
                                              $ orderCommentsAndCommentables (commentable      parsed  )
                                                                             (classifyComments comments)
                                     return   $ Right (getPointLoc <$> parsed,
                                                       classifyComments comments)
    ParseFailed aLoc msg       ->    return   $ Left $ critical aLoc msg
  where
    handleException helper (e :: SomeException) = return $ helper $ show e
    thisFileLoc = noLoc { srcFilename = inputFilename }
    parseMode = ParseMode {
                  parseFilename         = inputFilename
                , baseLanguage          = Haskell2010
                , extensions            = myExtensions
                , ignoreLanguagePragmas = False
                , ignoreLinePragmas     = False
                , fixities              = Just preludeFixities
                , ignoreFunctionArity   = False
                }
{-putStrLn   "COMMENTS:"
                                     putStrLn $ unlines $ map show $ classifyComments comments
                                     putStrLn   "COMMENTABLES:"
                                     putStrLn $ unlines $ map show $ commentable      parsed-}
                                     
