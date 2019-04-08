{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module GRIFT.Semantics.Pretty
  ( -- ** Pretty printing
    AbbrevLevel(..)
  , pPrintInstSemantics
  ) where

import Control.Lens ((^.))
import Data.Foldable (toList)
import Data.Parameterized
import Data.Parameterized.List
import Prelude hiding ((<>), (!!))
import Text.PrettyPrint.HughesPJClass

import GRIFT.Semantics
import GRIFT.Semantics.Expand
import GRIFT.Types

-- | Flag indicating whether to expand abbreviated statements and expressions.
data AbbrevLevel = Abbrev | NoAbbrev
  deriving (Eq, Show)

pPrintLocApp :: (forall w' . Bool -> expr w' -> Doc)
             -> Bool
             -> LocApp expr rv w
             -> Doc
pPrintLocApp _ _ (PCApp _) = text "pc"
pPrintLocApp ppExpr top (GPRApp _ e) = text "xRaw[" <> ppExpr top e <> text "]" -- this should not happen
pPrintLocApp ppExpr top (FPRApp _ e) = text "f[" <> ppExpr top e <> text "]"
pPrintLocApp ppExpr top (MemApp bytes e) = text "M[" <> ppExpr top e <> text "]_" <> pPrint (intValue bytes)
pPrintLocApp ppExpr top (ResApp e) = text "MReserved[" <> ppExpr top e <> text "]"
pPrintLocApp ppExpr top (CSRApp _ e) = text "CSRRaw[" <> ppExpr top e <> text "]"
pPrintLocApp _ _ PrivApp = text "current_priv"

pPrintAbbrevApp :: KnownRV rv
                => (forall w' . Bool -> expr rv w' -> Doc)
                -> Bool
                -> AbbrevApp expr rv w
                -> Doc
pPrintAbbrevApp ppExpr _ (SafeGPRApp _ e) = text "x[" <> ppExpr True e <> text "]"
pPrintAbbrevApp ppExpr _ (ReadCSRApp _ e) = text "CSR[" <> ppExpr True e <> text "]"
pPrintAbbrevApp ppExpr _ (NanBox32App _ e) = text "NaNBox32(" <> ppExpr True e <> text ")"
pPrintAbbrevApp ppExpr _ (UnNanBox32App _ e) = text "UnNaNBox32(" <> ppExpr True e <> text ")"

pPrintStateApp :: (forall w' . Bool -> expr w' -> Doc)
               -> Bool
               -> StateApp expr rv w
               -> Doc
pPrintStateApp ppExpr top (LocApp loc) = pPrintLocApp ppExpr top loc
pPrintStateApp ppExpr top (AppExpr app) = pPrintBVApp ppExpr top app
pPrintStateApp ppExpr top (FloatAppExpr app) = pPrintBVFloatApp ppExpr top app

pPrintBVApp :: (forall w' . Bool -> expr w' -> Doc)
            -> Bool
            -> BVApp expr w
            -> Doc
pPrintBVApp ppExpr _ (NotApp _ e) = text "!" <> ppExpr False e
pPrintBVApp ppExpr _ (NegateApp _ e) = text "-" <> ppExpr False e
pPrintBVApp ppExpr _ (AbsApp _ e) = text "|" <> ppExpr True e <> text "|"
pPrintBVApp ppExpr _ (SignumApp _ e) = text "signum(" <> ppExpr True e <> text ")"
pPrintBVApp ppExpr _ (ZExtApp _ e) = text "zext(" <> ppExpr True e <> text ")"
pPrintBVApp ppExpr _ (SExtApp _ e) = text "sext(" <> ppExpr True e <> text ")"
pPrintBVApp ppExpr _ (ExtractApp w ix e) =
  ppExpr False e <> text "[" <> pPrint (intValue ix) <> text ":" <>
  pPrint (intValue ix + intValue w - 1) <> text "]"
pPrintBVApp ppExpr False e = parens (pPrintBVApp ppExpr True e)
pPrintBVApp ppExpr _ (AndApp _ e1 e2) = ppExpr False e1 <+> text "&" <+> ppExpr False e2
pPrintBVApp ppExpr _ (OrApp _  e1 e2) = ppExpr False e1 <+> text "|" <+> ppExpr False e2
pPrintBVApp ppExpr _ (XorApp _ e1 e2) = ppExpr False e1 <+> text "^" <+> ppExpr False e2
pPrintBVApp ppExpr _ (SllApp _ e1 e2) = ppExpr False e1 <+> text "<<" <+> ppExpr False e2
pPrintBVApp ppExpr _ (SrlApp _ e1 e2) = ppExpr False e1 <+> text ">l>" <+> ppExpr False e2
pPrintBVApp ppExpr _ (SraApp _ e1 e2) = ppExpr False e1 <+> text ">a>" <+> ppExpr False e2
pPrintBVApp ppExpr _ (AddApp _ e1 e2) = ppExpr False e1 <+> text "+" <+> ppExpr False e2
pPrintBVApp ppExpr _ (SubApp _ e1 e2) = ppExpr False e1 <+> text "-" <+> ppExpr False e2
pPrintBVApp ppExpr _ (MulApp _ e1 e2) = ppExpr False e1 <+> text "*" <+> ppExpr False e2
pPrintBVApp ppExpr _ (QuotUApp _ e1 e2) = ppExpr False e1 <+> text "/u" <+> ppExpr False e2
pPrintBVApp ppExpr _ (QuotSApp _ e1 e2) = ppExpr False e1 <+> text "/s" <+> ppExpr False e2
pPrintBVApp ppExpr _ (RemUApp _ e1 e2) = ppExpr False e1 <+> text "%u" <+> ppExpr False e2
pPrintBVApp ppExpr _ (RemSApp _ e1 e2) = ppExpr False e1 <+> text "%s" <+> ppExpr False e2
pPrintBVApp ppExpr _ (EqApp  e1 e2) = ppExpr False e1 <+> text "==" <+> ppExpr False e2
pPrintBVApp ppExpr _ (LtuApp e1 e2) = ppExpr False e1 <+> text "<u" <+> ppExpr False e2
pPrintBVApp ppExpr _ (LtsApp e1 e2) = ppExpr False e1 <+> text "<s" <+> ppExpr False e2
pPrintBVApp ppExpr _ (ConcatApp _ e1 e2) =
  text "{" <> ppExpr True e1 <> text ", " <> ppExpr True e2 <> text "}"
pPrintBVApp ppExpr _ (IteApp _ e1 e2 e3) =
  text "if" <+> ppExpr True e1 <+>
  text "then" <+> ppExpr True e2 <+>
  text "else" <+> ppExpr True e3

pPrintBVFloatApp :: (forall w' . Bool -> expr w' -> Doc)
            -> Bool
            -> BVFloatApp expr w
            -> Doc

pPrintBVFloatApp ppExpr _ (Ui32ToF16App rm e) =
  text "ui32ToF16(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (Ui32ToF32App rm e) =
  text "ui32ToF32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (Ui32ToF64App rm e) =
  text "ui32ToF64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (I32ToF16App rm e) =
  text "i32ToF16(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (I32ToF32App rm e) =
  text "i32ToF32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (I32ToF64App rm e) =
  text "i32ToF64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (Ui64ToF16App rm e) =
  text "ui64ToF16(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (Ui64ToF32App rm e) =
  text "ui64ToF32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (Ui64ToF64App rm e) =
  text "ui64ToF64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (I64ToF16App rm e) =
  text "i64ToF16(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (I64ToF32App rm e) =
  text "i64ToF32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (I64ToF64App rm e) =
  text "i64ToF64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"

pPrintBVFloatApp ppExpr _ (F16ToUi32App rm e) =
  text "f16ToUi32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16ToUi64App rm e) =
  text "f16ToUi64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16ToI32App rm e) =
  text "f16ToI32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16ToI64App rm e) =
  text "f16ToI64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32ToUi32App rm e) =
  text "f32ToUi32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32ToUi64App rm e) =
  text "f32ToUi64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32ToI32App rm e) =
  text "f32ToI32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32ToI64App rm e) =
  text "f32ToI64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64ToUi32App rm e) =
  text "f64ToUi32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64ToUi64App rm e) =
  text "f64ToUi64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64ToI32App rm e) =
  text "f64ToI32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64ToI64App rm e) =
  text "f64ToI64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"

pPrintBVFloatApp ppExpr _ (F16ToF32App rm e) =
  text "f16ToF32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16ToF64App rm e) =
  text "f16ToF64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32ToF16App rm e) =
  text "f32ToF16(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32ToF64App rm e) =
  text "f32ToF64(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64ToF16App rm e) =
  text "f64ToF16(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64ToF32App rm e) =
  text "f64ToF32(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"

pPrintBVFloatApp ppExpr _ (F16RoundToIntApp rm e) =
  text "f16RoundToInt(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16AddApp rm x y) =
  text "f16Add(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16SubApp rm x y) =
  text "f16Sub(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16MulApp rm x y) =
  text "f16Mul(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16MulAddApp rm x y z) =
  text "f16MulAdd(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", " <> ppExpr True z <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16DivApp rm x y) =
  text "f16Div(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16RemApp rm x y) =
  text "f16Rem(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16SqrtApp rm e) =
  text "f16Sqrt(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F16EqApp x y) =
  text "f16Eq(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F16LeApp x y) =
  text "f16Le(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F16LtApp x y) =
  text "f16Lt(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F16EqSignalingApp x y) =
  text "f16EqSignaling(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F16LeQuietApp x y) =
  text "f16LeQuiet(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F16LtQuietApp x y) =
  text "f16LtQuiet(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F16IsSignalingNaNApp x) =
  text "f16IsSignalingNaN(" <> ppExpr True x <>  text ")"

pPrintBVFloatApp ppExpr _ (F32RoundToIntApp rm e) =
  text "f32RoundToInt(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32AddApp rm x y) =
  text "f32Add(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32SubApp rm x y) =
  text "f32Sub(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32MulApp rm x y) =
  text "f32Mul(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32MulAddApp rm x y z) =
  text "f32MulAdd(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", " <> ppExpr True z <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32DivApp rm x y) =
  text "f32Div(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32RemApp rm x y) =
  text "f32Rem(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32SqrtApp rm e) =
  text "f32Sqrt(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F32EqApp x y) =
  text "f32Eq(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F32LeApp x y) =
  text "f32Le(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F32LtApp x y) =
  text "f32Lt(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F32EqSignalingApp x y) =
  text "f32EqSignaling(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F32LeQuietApp x y) =
  text "f32LeQuiet(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F32LtQuietApp x y) =
  text "f32LtQuiet(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F32IsSignalingNaNApp x) =
  text "f32IsSignalingNaN(" <> ppExpr True x <>  text ")"

pPrintBVFloatApp ppExpr _ (F64RoundToIntApp rm e) =
  text "f64RoundToInt(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64AddApp rm x y) =
  text "f64Add(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64SubApp rm x y) =
  text "f64Sub(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64MulApp rm x y) =
  text "f64Mul(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64MulAddApp rm x y z) =
  text "f64MulAdd(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", " <> ppExpr True z <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64DivApp rm x y) =
  text "f64Div(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64RemApp rm x y) =
  text "f64Rem(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64SqrtApp rm e) =
  text "f64Sqrt(" <> ppExpr True e <> text ", RM=" <> ppExpr True rm <> text ")"
pPrintBVFloatApp ppExpr _ (F64EqApp x y) =
  text "f64Eq(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F64LeApp x y) =
  text "f64Le(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F64LtApp x y) =
  text "f64Lt(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F64EqSignalingApp x y) =
  text "f64EqSignaling(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F64LeQuietApp x y) =
  text "f64LeQuiet(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F64LtQuietApp x y) =
  text "f64LtQuiet(" <> ppExpr True x <> text ", " <> ppExpr True y <> text ")"
pPrintBVFloatApp ppExpr _ (F64IsSignalingNaNApp x) =
  text "f64IsSignalingNaN(" <> ppExpr True x <>  text ")"

pPrintAbbrevStmt :: (forall w' . Bool -> expr rv w' -> Doc)
                 -> AbbrevStmt expr rv
                 -> Doc
pPrintAbbrevStmt ppExpr (SafeGPRAssign ridE e) =
  text "x[" <> ppExpr True ridE <> text "] :=" <+> ppExpr True e
pPrintAbbrevStmt ppExpr (RaiseException code info) =
  text "raiseException(code = " <> text (show code) <> text ", info = " <> ppExpr True info <> text")"
pPrintAbbrevStmt ppExpr (WriteCSR csr e) =
  text "CSR[" <> ppExpr True csr <> text "] := " <> ppExpr True e

pPrintStmt :: KnownRV rv
           => (forall w' . Bool -> expr rv w' -> Doc)
           -> AbbrevLevel
           -> Stmt expr rv
           -> Doc
pPrintStmt ppExpr _ (AssignStmt le e) = pPrintLocApp ppExpr True le <+> text ":=" <+> ppExpr True e
pPrintStmt ppExpr abbrevLevel (AbbrevStmt abbrevStmt)
  | abbrevLevel == Abbrev = pPrintAbbrevStmt ppExpr abbrevStmt
  | otherwise = vcat (pPrintStmt ppExpr abbrevLevel <$> toList (expandAbbrevStmt abbrevStmt))
pPrintStmt ppExpr abbrevLevel (BranchStmt test s1s s2s) =
  text "IF" <+> ppExpr True test
  $$ nest 2 (text "THEN")
  $$ nest 4 (vcat (pPrintStmt ppExpr abbrevLevel <$> toList s1s))
  $$ nest 2 (text "ELSE")
  $$ nest 4 (vcat (pPrintStmt ppExpr abbrevLevel <$> toList s2s))

pPrintSemantics :: KnownRV rv
                => (forall w' . Bool -> expr rv w' -> Doc)
                -> AbbrevLevel
                -> Semantics expr rv
                -> Doc
pPrintSemantics ppExpr abbrevLevel semantics =
  (vcat $ text <$> toList (semantics ^. semComments)) $$
  text "" $$
  (vcat $ pPrintStmt ppExpr abbrevLevel <$> toList (semantics ^. semStmts))

pPrintOperandName :: OperandName w -> Doc
pPrintOperandName Aq = text "aq"
pPrintOperandName Rl = text "rl"
pPrintOperandName Rm = text "rm"
pPrintOperandName Rd = text "rd"
pPrintOperandName Rs1 = text "rs1"
pPrintOperandName Rs2 = text "rs2"
pPrintOperandName Rs3 = text "rs3"
pPrintOperandName Imm5 = text "imm5"
pPrintOperandName Shamt5 = text "shamt5"
pPrintOperandName Shamt7 = text "shamt7"
pPrintOperandName Imm12 = text "imm12"
pPrintOperandName Csr = text "csr"
pPrintOperandName Imm20 = text "imm20"
pPrintOperandName Imm32 = text "imm32"

-- | Pretty-print an 'InstExpr'.
pPrintInstExpr :: KnownRV rv
               => List OperandName (OperandTypes fmt)
               -- ^ Names for each operand of the expression
               -> AbbrevLevel
               -> Bool
               -- ^ Precedence flag -- 'True' means we don't need parentheses (for
               -- example, if the expression is already bracketed by a containing
               -- expression)
               -> InstExpr fmt rv w
               -> Doc
pPrintInstExpr opNames _ _ (OperandExpr _ (OperandID oid)) = pPrintOperandName (opNames !! oid)
pPrintInstExpr _ _ _ (InstLitBV bv) = text (show bv)
pPrintInstExpr opNames abbrevLevel top (InstAbbrevApp abbrevApp)
  | abbrevLevel == Abbrev = pPrintAbbrevApp (pPrintInstExpr opNames abbrevLevel) top abbrevApp
  | otherwise = pPrintInstExpr opNames abbrevLevel top (expandAbbrevApp abbrevApp)
pPrintInstExpr _ _ _ (InstBytes _) = text "step"
pPrintInstExpr _ _ _ (InstWord _) = text "inst"
pPrintInstExpr opNames abbrevLevel top (InstStateApp e) =
  pPrintStateApp (pPrintInstExpr opNames abbrevLevel) top e

-- ^ Pretty-print all the statements in the semantics of an instruction, along with
-- the comments.
pPrintInstSemantics :: KnownRV rv => AbbrevLevel -> InstSemantics rv fmt -> Doc
pPrintInstSemantics abbrevLevel (InstSemantics semantics opNames) =
  pPrintSemantics (pPrintInstExpr opNames abbrevLevel) abbrevLevel semantics
