module Homplexity where

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts

getDecls (Module _ _ _ _ _ _ decls) = decls

isFunBind (FunBind _) = True
isFunBind _           = False

funBinds = filter isFunBind
         . getDecls

main :: IO ()
main = do
  ParseOk r <- parseFile "Test.hs"
  print $ funBinds r 

