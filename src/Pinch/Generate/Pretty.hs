{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Pinch.Generate.Pretty where

import qualified Data.Char                 as Char
import           Data.String
import qualified Data.Text                 as T
import           Prelude hiding (mod)
import           Prettyprinter
import Data.List (nub)

newtype ModuleName = ModuleName T.Text
  deriving (Show)
type TypeName = T.Text
type Name = T.Text
type ClassName = T.Text

data Module = Module
  { modName    :: ModuleName
  , modExports :: Exports
  , modPragmas :: [Pragma]
  , modImports :: [ImportDecl]
  , modDecls   :: [Decl]
  }
  deriving (Show)

data Pragma
  = PragmaLanguage T.Text
  | PragmaOptsGhc T.Text
  deriving (Show)

data ExportDataConstructors
  = NoConstructors
  | Constructors [Name]
  | AllConstructors
  deriving Show

data Export
  = ExportType Name ExportDataConstructors
  | ExportFunction Name
  deriving Show

newtype Exports = Exports (Maybe [Export])
  deriving Show

data ImportDecl = ImportDecl
  { iName      :: ModuleName
  , iQualified :: Bool
  , iThings    :: ImportNames
  }
  deriving (Show)

data ImportNames
  = IEverything
  | IJust [ Name ]
  deriving (Show)

data FunDep = FunDep Name Name
  deriving Show

data Decl
  = TypeDecl Type Type
  | DataDecl TypeName [Name] [ConDecl] [Deriving]
  | ClassDecl Name [TypeParam] [FunDep] [(Name, Type)]
  | InstDecl InstHead [Decl]
  | FunBind [Match]
  | TypeSigDecl [Constraint] Name Type
  | ClosedTypeFamily Name [Name] [([Pat], Exp)]
  deriving (Show)

data TypeParam
  = TypeParam Name
  | TypeParamAnn Name Type
  deriving Show

instance Pretty TypeParam where
  pretty (TypeParam n) = pretty n
  pretty (TypeParamAnn n t) = parens $ hsep [pretty n, "::", pretty t]

instance IsString TypeParam where
  fromString = TypeParam . T.pack

data Deriving
  = DeriveClass Type
  deriving (Show)

data ConDecl
  = ConDecl Name [Type]
  | RecConDecl Name [(Name, Type)]
  deriving (Show)

instance IsString ConDecl where
  fromString a = ConDecl (T.pack a) []

data Type
  = TyApp Type [Type]
  | TyCon TypeName
  | TyLam [Type] Type
  | TyTup [Type]
  | TyAnn Type Type
  deriving (Eq, Show)

instance IsString Type where
  fromString = TyCon . T.pack

data InstHead
  = InstHead [Constraint] ClassName [Type]
  deriving (Show)

data Constraint
  = CClass ClassName [Type]
  deriving (Eq, Show)

data Match = Match Name [Pat] Exp
  deriving (Show)

data Pat
  = PVar Name
  | PLit Lit
  | PCon Name [Pat]
  deriving (Show)

data Exp
  = EVar Name
  | EApp Exp [Exp]
  | ELit Lit
  | ETyAnn Exp Type
  | ECase Exp [Alt]
  | EDo [Stm]
  | EInfix Name Exp Exp
  | EList [Exp]
  | ELam [Pat] Exp
  | ETuple [Exp]
  | ETupleSection [Maybe Exp]
  | ELet Name Exp Exp
  | ETyApp Exp [Type]
  deriving (Show)

data Stm
  = StmBind (Maybe Pat) Exp
  deriving (Show)

data Alt
  = Alt Pat Exp
  deriving (Show)

data Lit
  = LInt Integer
  | LFloat Double
  | LString T.Text
  deriving (Show)

instance Pretty ModuleName where
  pretty (ModuleName x) = pretty x

instance Pretty Module where
  pretty mod =
       vsep (map pretty $ modPragmas mod) <> line <> line
    <> "module" <+> pretty (modName mod)
    <> line
    <> indent 2 (pretty (modExports mod) <> line <> "where")
    <> line <> line
    <> vsep (map pretty $ modImports mod) <> line <> line
    <> vsep (map pretty $ modDecls mod)

instance Pretty Exports where
  pretty (Exports Nothing) = mempty
  pretty (Exports (Just expts)) = encloseSep "(" (line <> ")") "," ((" " <>) . pretty <$> expts)

instance Pretty Export where
  pretty a = case a of
    ExportType name constructors -> pretty name <> case constructors of
      NoConstructors -> mempty
      Constructors constructors' -> tupled $ pretty <$> constructors'
      AllConstructors -> "(..)"
    ExportFunction name -> pretty name


instance Pretty Pragma where
  pretty p = case p of
    PragmaLanguage p' -> "{-# LANGUAGE" <+> pretty p' <+> "#-}"
    PragmaOptsGhc o -> "{-# OPTIONS_GHC" <+> pretty o <+> "#-}"

instance Pretty ImportDecl where
  pretty i = "import" <+> (if (iQualified i) then "qualified" else "") <+> pretty (iName i) <> pretty (iThings i)

instance Pretty ImportNames where
  pretty i = case i of
    IEverything -> ""
    IJust xs -> " " <> (parens $ cList $ map pretty xs)

instance Pretty Decl where
  pretty decl = case decl of
    TypeDecl t1 t2 -> hang 2 $ hsep (["type", pretty t1] <> (pretty <$> nub (collectTypeVars t2)) <> ["="]) <> softline <> pretty t2 <> line
    DataDecl t typarams [] ds -> "data" <+> pretty t <+> hsep (pretty <$> typarams) <+> prettyDerivings ds <> line
    DataDecl t typarams (c:cs) ds -> nest 2 (vsep $
        [ "data" <+> pretty t <+> hsep (pretty <$> typarams)
        , "=" <+> pretty c
        ] ++ (map (\c' -> "|" <+> pretty c') cs) ++ [ prettyDerivings ds ]
      ) <> line
    InstDecl h decls -> (nest 2 $ vsep $ [ pretty h ] ++ map pretty decls) <> line
    FunBind ms -> vsep (map pretty ms) <> line
    TypeSigDecl constraints n ty -> nest 2 $ hsep $ concat
        [ [pretty n, "::"]
        , prettyForall (concatMap constraintTypeVars constraints <> collectTypeVars ty)
        , prettyConstraints constraints
        , [pretty ty]
        ]
    ClosedTypeFamily name params matches ->
      nest 2 $ vsep
        [ "type family" <+> pretty name <+> hsep (pretty <$> params) <+> "where"
        , vsep ((pretty name <+>) . prettyTyFamMatch <$> matches)
        ] <> line
    ClassDecl className params fundeps methods ->
      (<> line) $ hang 2 $ vsep $ (hsep $ concat
        [ ["class", pretty className]
        , pretty <$> params
        , if null fundeps
          then []
          else ["|", concatWith (\a b -> a <> "," <+> b) $ pretty <$> fundeps]
        , ["where"]
        ]) : fmap prettyClassMethod methods

prettyForall :: [Name] -> [Doc a]
prettyForall vars = case filter (not . T.null) vars of
  [] -> []
  vars' -> [(<> ".") $ hsep $ "forall" : fmap pretty (nub vars')]

constraintTypeVars :: Constraint -> [Name]
constraintTypeVars (CClass _ ts) = concatMap collectTypeVars ts

collectTypeVars :: Type -> [Name]
collectTypeVars t = case t of
  TyApp t1 ts -> concatMap collectTypeVars (t1 : ts)
  TyCon n | (Char.isLower . fst <$> T.uncons n) == Just True -> [n]
          | otherwise -> []
  TyLam ts t1 -> concatMap collectTypeVars (t1 : ts)
  TyTup ts -> concatMap collectTypeVars ts
  TyAnn t1 t2 -> collectTypeVars t1 <> collectTypeVars t2

prettyClassMethod :: (Name, Type) -> Doc a
prettyClassMethod (name, t) = hsep [pretty name, "::", pretty t]

instance Pretty FunDep where
  pretty (FunDep a b) = hsep [pretty a, "->", pretty b]

prettyTyFamMatch :: ([Pat], Exp) -> Doc a
prettyTyFamMatch (pats, expr) = hsep (pretty <$> pats) <+> "=" <+> pretty expr

prettyDerivings :: [Deriving] -> Doc a
prettyDerivings [] = ""
prettyDerivings ds = "deriving" <+> (parens $ cList $ map pretty ds)

instance Pretty Deriving where
  pretty (DeriveClass c) = pretty c

instance Pretty ConDecl where
  pretty (ConDecl n args) = hsep $ [ pretty n ] ++ map pretty args
  pretty (RecConDecl n fields) = pretty n
    <> case fields of
      [] -> "{}"
      ((f, t) : xs) -> line
        <> "{" <+> pretty f <+> "::" <+> pretty t
          <> line
          <> vsep (map (\(f', v) -> "," <+> pretty f' <+> "::" <+> pretty v) xs)
          <> line
          <> "}"

instance Pretty InstHead where
  pretty (InstHead cs n ty) = hsep $ concat [["instance"], prettyConstraints cs, [pretty n], pretty <$> ty, ["where"]]

prettyConstraints :: [Constraint] -> [Doc ann]
prettyConstraints cs =
  if null cs
  then []
  else [parens (cList $ map pretty cs) <+> "=>"]

instance Pretty Constraint where
  pretty (CClass cl []) = pretty cl
  pretty (CClass cl [t]) = pretty cl <+> pretty t
  pretty (CClass cl ts) = pretty cl <+> hsep (pretty <$> ts)

instance Pretty Type where
  pretty ty = case ty of
    TyApp t1 ts -> parens $ pretty t1 <+> hsep (map pretty ts)
    TyCon t -> pretty t
    TyLam ts t -> concatWith (surround (space <> "->" <> space)) (map (parens . pretty) ts ++ [pretty t])
    TyTup els -> nest 2 $ tupled $ pretty <$> els
    TyAnn a b -> hsep [pretty a, "::", pretty b]

instance Pretty Match where
  pretty (Match n ps e) = pretty n <+> hsep (map pretty ps) <+> "=" <+> pretty e

instance Pretty Pat where
  pretty p = case p of
    (PVar x) -> pretty x
    (PLit i) -> pretty i
    (PCon n []) -> pretty n
    (PCon n xs) -> parens $ pretty n <+> hsep (map pretty xs)

instance Pretty Exp where
  pretty e = case e of
    EVar n -> pretty n
    EApp e' es -> pretty e' <+> hsep (map (parens . pretty) es)
    ELit l -> pretty l
    ETyAnn e' ty -> parens $ pretty e' <+> "::" <+> pretty ty
    ECase e' as -> nest 2 $ vsep $ ["case" <+> pretty e' <+> "of"] ++ map pretty as
    EDo s -> nest 2 $ vsep $ ["do"] ++ map pretty s
    EInfix op e1 e2 -> parens $ hsep [ pretty e1, pretty op, pretty e2]
    EList es -> "[" <+> cList (map pretty es) <+> "]"
    ELam ps e' -> parens $ "\\" <> hsep (map pretty ps) <+> "->" <+> pretty e'
    ETuple es -> nest 2 $ tupled $ map pretty es
    ELet nm e1 e2 -> "let" <+> pretty nm <+> "=" <+> indent 2 (pretty e1) <+> "in" <+> pretty e2
    ETyApp e' tys -> pretty e' <+>  hsep (map (("@"<>) . parens . pretty) tys)
    ETupleSection es -> tupled $ maybe mempty pretty <$> es

instance Pretty Alt where
  pretty (Alt p e) = pretty p <+> "->" <+> pretty e

instance Pretty Stm where
  pretty s = case s of
    StmBind Nothing e -> pretty e
    StmBind (Just p) e -> pretty p <+> "<-" <+> pretty e

instance Pretty Lit where
  pretty l = case l of
    LInt i -> pretty i
    LFloat f -> pretty f
    LString t -> "\"" <> pretty t <> "\""

cList :: [Doc ann] -> Doc ann
cList = concatWith (surround (comma <> space))


instance IsString Exp where
  fromString = EVar . T.pack

instance IsString Pat where
  fromString = PVar . T.pack
