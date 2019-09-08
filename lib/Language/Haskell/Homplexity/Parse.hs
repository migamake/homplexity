{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Parsing of Haskell source files, and error reporting for unparsable files.
module Language.Haskell.Homplexity.Parse (parseSource) where

import Control.Exception as E
import Data.Functor
import Data.Maybe
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict as Map

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts
import Language.Haskell.Homplexity.Comments
import Language.Haskell.Homplexity.Message
import Language.Preprocessor.Cpphs

--import HFlags


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


-- | Removes duplicate and switching extensions.
--
--   Example:
--
--   >>> [ EnableExtension ScopedTypeVariables, DisableExtension ScopedTypeVariables, EnableExtension DoRec ]
--   [ DisableExtension ScopedTypeVariables, EnableExtension DoRec ]
--
collapseSameExtensions :: [Extension] -> [Extension]
collapseSameExtensions = mkList . foldl processExtension Map.empty
  where
    processExtension :: Map KnownExtension Bool -> Extension -> Map KnownExtension Bool
    processExtension m (UnknownExtension _) = m
    processExtension m (EnableExtension  e) = Map.insert e True  m
    processExtension m (DisableExtension e) = Map.insert e False m
    mkList = map (\case (e, True)  -> EnableExtension e
                        (e, False) -> DisableExtension e
                 )
             . Map.toList


-- | Parse Haskell source file, using CppHs for preprocessing,
-- and haskell-src-exts for parsing.
--
-- Catches all exceptions and wraps them as @Critical@ log messages.
parseSource :: [Extension] -> FilePath -> IO (Either Log (Module SrcLoc, [CommentLink]))
parseSource additionalExtensions inputFilename = do
  parseResult <- (do
    input        <- readFile inputFilename
    deCppHsInput <- runCpphs cppHsOptions inputFilename input
    let fileExtensions = maybe [] snd $ readExtensions deCppHsInput
        extensions     = collapseSameExtensions (additionalExtensions ++ fileExtensions)
        result         = parseModuleWithComments (parseMode extensions) deCppHsInput
    evaluate result)
      `E.catch` handleException (ParseFailed thisFileLoc)
  case parseResult of
    ParseOk (parsed, comments) -> do {-putStrLn   "ORDERED:"
                                     putStrLn $ unlines $ map show
                                              $ orderCommentsAndCommentables (commentable      parsed  )
                                                                             (classifyComments comments) -}
                                     return   $ Right (getPointLoc <$> parsed,
                                                       classifyComments comments) 
    ParseFailed aLoc msg       ->    return   $ Left $ critical aLoc msg
  where
    handleException helper (e :: SomeException) = return $ helper $ show e
    thisFileLoc = noLoc { srcFilename = inputFilename }
    parseMode extensions = ParseMode {
                  parseFilename         = inputFilename
                , baseLanguage          = Haskell2010
                , extensions            = extensions
                , ignoreLanguagePragmas = False
                , ignoreLinePragmas     = False
                , fixities              = Just preludeFixities
                , ignoreFunctionArity   = False
                }
{-putStrLn   "COMMENTS:"
                                     putStrLn $ unlines $ map show $ classifyComments comments
                                     putStrLn   "COMMENTABLES:"
                                     putStrLn $ unlines $ map show $ commentable      parsed-}
                                     
