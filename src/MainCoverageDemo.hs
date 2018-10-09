{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Control.Lens ((^.))
import Data.BitVector.Sized.App
import Data.Foldable
import Data.Maybe
import qualified Data.Parameterized.Map as Map
import Data.Parameterized.List
import Data.Parameterized.TraversableFC
import GHC.TypeLits
import Text.PrettyPrint.HughesPJClass

import RISCV.InstructionSet
import RISCV.InstructionSet.Known
import RISCV.Semantics
import RISCV.Types

-- | A 'CTNode' contains an expression and a flag indicating whether or not that
-- expression has been evaluated.
data CTNode (expr :: Nat -> *) (w :: Nat) = CTNode Bool (expr w)

data CT (expr :: Nat -> *) = CT (expr 1) [CT expr] [CT expr] [CT expr]

pPrintCT :: List OperandName (OperandTypes fmt)
               -> CT (InstExpr fmt rv)
               -> Doc
pPrintCT opNames (CT e [] [] []) = pPrintInstExpr opNames True e
pPrintCT opNames (CT e t l r) =
  pPrintInstExpr opNames True e
  $$ nest 2 (text "?>" <+> vcat (pPrintCT opNames <$> t))
  $$ nest 2 (text "t>" <+> vcat (pPrintCT opNames <$> l))
  $$ nest 2 (text "f>" <+> vcat (pPrintCT opNames <$> r))

coverageTreeLocApp :: LocApp (InstExpr fmt rv) rv w -> [CT (InstExpr fmt rv)]
coverageTreeLocApp (RegExpr e) = coverageTreeInstExpr e
coverageTreeLocApp (FRegExpr e) = coverageTreeInstExpr e
coverageTreeLocApp (MemExpr _ e) = coverageTreeInstExpr e
coverageTreeLocApp (ResExpr e) = coverageTreeInstExpr e
coverageTreeLocApp (CSRExpr e) = coverageTreeInstExpr e
coverageTreeLocApp _ = []

coverageTreeStateApp :: StateApp (InstExpr fmt rv) rv w -> [CT (InstExpr fmt rv)]
coverageTreeStateApp (LocApp e) = coverageTreeLocApp e
coverageTreeStateApp (AppExpr e) = coverageTreeBVApp e

coverageTreeInstExpr :: InstExpr fmt rv w -> [CT (InstExpr fmt rv)]
coverageTreeInstExpr (InstStateExpr e) = coverageTreeStateApp e
coverageTreeInstExpr _ = []

coverageTreeBVApp :: BVApp (InstExpr fmt rv) w -> [CT (InstExpr fmt rv)]
coverageTreeBVApp (IteApp t l r) =
  [CT t (coverageTreeInstExpr t) (coverageTreeInstExpr l) (coverageTreeInstExpr r)]
coverageTreeBVApp app = foldMapFC coverageTreeInstExpr app

coverageTreeStmt :: Stmt (InstExpr fmt rv) rv -> [CT (InstExpr fmt rv)]
coverageTreeStmt (AssignStmt _ e) = coverageTreeInstExpr e
coverageTreeStmt (BranchStmt t l r) =
  let tTrees = coverageTreeInstExpr t
      lTrees = concat $ toList $ coverageTreeStmt <$> l
      rTrees = concat $ toList $ coverageTreeStmt <$> r
  in [CT t tTrees lTrees rTrees]

coverageTreeSemantics :: InstSemantics rv fmt -> [CT (InstExpr fmt rv)]
coverageTreeSemantics (InstSemantics sem _) =
  concat $ toList $ coverageTreeStmt <$> sem ^. semStmts

main :: IO ()
main = do
  let iset = knownISet :: InstructionSet RV64GC
      sem  = fromJust $ Map.lookup Bne (isSemanticsMap iset)
      cov  = coverageTreeSemantics sem
  print (pPrintInstSemantics sem)
  putStrLn ""
  traverse_ (print . pPrintCT (getOperandNames sem)) cov
