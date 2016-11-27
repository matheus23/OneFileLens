{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, QuasiQuotes #-}
module Data.Lens where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad
import Data.Function ((&))

data Lens wholeA wholeB subA subB
  = Lens
  { get    :: wholeA -> subA
  , modify :: (subA -> subB) -> (wholeA -> wholeB)
  }

liftModify :: Monad m => Lens wholeA wholeB subA subB -> (subA -> m subB) -> (wholeA -> m wholeB)
liftModify lens f whole = do
  sub <- f (get lens whole)
  return (whole & set lens sub)

set lens = modify lens . const
setIn whole lens value = whole & set lens value

whole = Lens id id

_1 = Lens fst (\ f (fst,snd) -> (f fst,snd))
_2 = Lens snd (\ f (fst,snd) -> (fst,f snd))

(-->) :: Lens a a b b -> Lens b b c c -> Lens a a c c
lensA --> lensB = Lens
  { get = get lensB . get lensA
  , modify = modify lensA . modify lensB }

{-
toStringL f = liftM $ LitE . StringL . f
showPretty = toStringL (show . ppr)
showIntern = toStringL show
-}

deriveLenses :: Name -> Q [Dec]
deriveLenses name = deriveLensesForInfo =<< reify name

deriveLensesForInfo :: Info -> Q [Dec]
deriveLensesForInfo (TyConI (DataD _ typeName _ _ [constructor] _))
  = deriveLensesForCon constructor
deriveLensesForInfo _ = return []

deriveLensesForCon :: Con -> Q [Dec]
deriveLensesForCon (RecC constr fields) = liftM concat $ mapM (deriveLensForField constr) fields
deriveLensesForCon _                    = return []

deriveLensForField :: Name -> VarStrictType -> Q [Dec]
deriveLensForField constr (fieldName,_,_) = do
  funName <- newName "f"
  recName <- newName "record"
  let fieldNameE = varE fieldName
      funP = varP funName
      funE = varE funName
      recP = varP recName
      recE = varE recName
  [d| $(varP lensName)
        = Lens $fieldNameE
               (\ $funP $recP ->
                  $(recUpdE recE [do
                     expr <- [| $funE ($fieldNameE $recE) |]
                     return (fieldName, expr)
                  ]) ) |]
  where
    lensName = mkName $ tail $ nameBase fieldName
