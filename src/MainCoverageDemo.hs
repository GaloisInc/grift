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
import Data.Parameterized.TraversableFC
import GHC.TypeLits
import Text.PrettyPrint.HughesPJClass

import RISCV.InstructionSet
import RISCV.InstructionSet.Known
import RISCV.Semantics
import RISCV.Types

data BoolTree (expr :: Nat -> *) = BoolTree (expr 1) [BoolTree expr] [BoolTree expr]

instance PrettyF expr => Pretty (BoolTree expr) where
  pPrint (BoolTree e [] []) = pPrintF e
  pPrint (BoolTree e l r) =
    pPrintF e $$
    nest 2 (text "t>" <+> vcat (pPrint <$> l)) $$
    nest 2 (text "f>" <+> vcat (pPrint <$> r))

coverageTreeLocApp :: LocApp (InstExpr fmt rv) rv w -> [BoolTree (InstExpr fmt rv)]
coverageTreeLocApp (RegExpr e) = coverageTreeInstExpr e
coverageTreeLocApp (FRegExpr e) = coverageTreeInstExpr e
coverageTreeLocApp (MemExpr _ e) = coverageTreeInstExpr e
coverageTreeLocApp (ResExpr e) = coverageTreeInstExpr e
coverageTreeLocApp (CSRExpr e) = coverageTreeInstExpr e
coverageTreeLocApp _ = []

coverageTreeStateApp :: StateApp (InstExpr fmt rv) rv w -> [BoolTree (InstExpr fmt rv)]
coverageTreeStateApp (LocApp e) = coverageTreeLocApp e
coverageTreeStateApp (AppExpr e) = coverageTreeBVApp e

coverageTreeInstExpr :: InstExpr fmt rv w -> [BoolTree (InstExpr fmt rv)]
coverageTreeInstExpr (InstStateExpr e) = coverageTreeStateApp e
coverageTreeInstExpr _ = []

coverageTreeBVApp :: BVApp (InstExpr fmt rv) w -> [BoolTree (InstExpr fmt rv)]
coverageTreeBVApp (IteApp t l r) = [BoolTree t (coverageTreeInstExpr l) (coverageTreeInstExpr r)]
coverageTreeBVApp app = foldMapFC coverageTreeInstExpr app

coverageTreeStmt :: Stmt (InstExpr fmt rv) rv -> [BoolTree (InstExpr fmt rv)]
coverageTreeStmt (AssignStmt _ e) = coverageTreeInstExpr e
coverageTreeStmt (BranchStmt t l r) =
  let lTrees = concat $ toList $ coverageTreeStmt <$> l
      rTrees = concat $ toList $ coverageTreeStmt <$> r
  in [BoolTree t lTrees rTrees]

coverageTreeSemantics :: Semantics (InstExpr fmt rv) rv -> [BoolTree (InstExpr fmt rv)]
coverageTreeSemantics sem = concat $ toList $ coverageTreeStmt <$> sem ^. semStmts

main :: IO ()
main = do
  let iset = knownISet :: InstructionSet RV64GC
      sem  = getInstSemantics $ fromJust $ Map.lookup Slli (isSemanticsMap iset)
      cov  = coverageTreeSemantics sem
  print (pPrint sem)
  putStrLn ""
  traverse_ (print . pPrint) cov
