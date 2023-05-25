{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}

module Pinch.Generate.DeclSCC
  ( declSCC
  ) where

import           GHC.Generics
import           Data.Foldable                         (toList)
import           Data.Graph                            (Graph)
import qualified Data.Graph                            as G
import           Data.Hashable
import qualified Data.HashMap.Strict                   as M
import           Data.HashMap.Strict                   (HashMap)
import           Data.Vector                           (Vector)
import qualified Data.Vector                           as V

import qualified Pinch.Generate.Pretty                 as H
import Debug.Trace

data HsBnd
  = TypeBnd H.Name
  | TermBnd H.Name
  deriving (Eq, Show, Generic, Hashable)

type HsEnv a = HashMap HsBnd a

declSCC :: [[H.Decl]] -> [[H.Decl]]
declSCC declsLL = fmap (declsV V.!) <$> sccInds
  where

    sccInds :: [[G.Vertex]]
    sccInds = toList <$> G.scc graph

    graph :: Graph
    (graph, _, _)
      = G.graphFromEdges
      $ zipWith mkEdges [0..] declsL

    mkEdges :: Int -> H.Decl -> (H.Decl, Int, [Int])
    mkEdges ind decl = (decl, ind, getDeclEdges namesToInds decl)

    declsL :: [H.Decl]
    declsL = concat declsLL

    namesToInds :: HsEnv Int
    namesToInds = surjective $ zip (topLevelBindings <$> declsL) [0..]

    declsV :: Vector H.Decl
    declsV = V.fromList declsL

surjective :: (Eq a, Hashable a) => [([a], b)] -> HashMap a b
surjective ksvs = M.fromList $ concatMap surList ksvs
  where
    surList (ks, v) = (, v) <$> ks

-------------
-- Create env
-------------

topLevelBindings :: H.Decl -> [HsBnd]
topLevelBindings decl = case decl of
  H.TypeDecl td -> topLevelTypeDeclBindings td
  H.InstDecl _ _ -> []
  H.FunBind matches -> topLevelFunBindings <$> matches
  H.TypeSigDecl _ _ -> []

topLevelTypeDeclBindings :: H.TypeDecl -> [HsBnd]
topLevelTypeDeclBindings node = case node of
  -- type alias can only introduce one binding
  H.TypedefDecl (H.TyCon name) _ -> [TypeBnd name]
  H.DataDecl name constructors _ -> TypeBnd name : fmap topLevelDataConstructorBindings constructors
  H.TypedefDecl _ _ -> error $ "The \"impossible\" happened! We made a type with no name.\n"
    <> "If only this library's internals were correct by construction..."

topLevelDataConstructorBindings :: H.ConDecl -> HsBnd
topLevelDataConstructorBindings node = case node of
  H.ConDecl name _ -> TermBnd name
  H.RecConDecl name _ -> TermBnd name

topLevelFunBindings :: H.Match -> HsBnd
topLevelFunBindings (H.Match name _ _) = TermBnd name

------------------
-- Get graph edges
------------------

accEnv :: HsEnv a -> HsBnd -> [a]
accEnv env bnd = case M.lookup bnd env of
  Nothing -> []
  Just a -> [a]

accTermEnv :: HsEnv a -> H.Name -> [a]
accTermEnv env name = accEnv env $ TermBnd name

accTypeEnv :: HsEnv a -> H.Name -> [a]
accTypeEnv env name = accEnv env $ TypeBnd name

getDeclEdges :: HsEnv a -> H.Decl -> [a]
getDeclEdges m decl = case decl of
  H.TypeDecl tyDecl -> getTypeDeclEdges m tyDecl
  H.InstDecl (H.InstHead _ _ t) decls -> concatMap (getDeclEdges m) decls <> getTypeEdges m t
  H.FunBind matches -> concatMap (getFunMatchEdges m) matches
  H.TypeSigDecl _ t -> getTypeEdges m t

getFunMatchEdges :: HsEnv a -> H.Match -> [a]
getFunMatchEdges m (H.Match _ pats expr)
  = concatMap (getPatEdges m) pats
  <> getExprEdges m expr

getPatEdges :: HsEnv a -> H.Pat -> [a]
getPatEdges env pat = case pat of
  H.PLit _ -> []
  H.PVar _ -> []
  H.PCon name pats -> accTermEnv env name <> concatMap (getPatEdges env) pats

getExprEdges :: HsEnv a -> H.Exp -> [a]
getExprEdges env node = case node of
  H.EVar a -> accTermEnv env a
  H.EApp a b -> rec a <> recs b
  H.ELit _ -> []
  H.ETyAnn expr ty -> rec expr <> getTypeEdges env ty
  H.ECase expr alts -> rec expr <> concatMap (getAltEdges env) alts
  H.EDo stmts -> concatMap (getStmtEdges env) stmts
  H.EInfix name e1 e2 -> accTermEnv env name <> rec e1 <> rec e2
  H.EList exprs -> recs exprs
  H.ELam pats expr -> concatMap (getPatEdges env) pats <> rec expr
  H.ETuple exprs -> recs exprs
  H.ELet _ e1 e2 -> rec e1 <> rec e2
  H.ETyApp expr ty -> rec expr <> concatMap (getTypeEdges env) ty
  where
    rec = getExprEdges env
    recs = concatMap rec

getAltEdges :: HsEnv a -> H.Alt -> [a]
getAltEdges env (H.Alt pat expr) = getPatEdges env pat <> getExprEdges env expr

getStmtEdges :: HsEnv a -> H.Stm -> [a]
getStmtEdges env (H.StmBind _ expr) = getExprEdges env expr

getTypeDeclEdges :: HsEnv a -> H.TypeDecl -> [a]
getTypeDeclEdges env node = case node of
  H.TypedefDecl _ t -> getTypeEdges env t
  H.DataDecl _ cons _ -> concatMap (getTypeConEdges env) cons

getTypeConEdges :: HsEnv a -> H.ConDecl -> [a]
getTypeConEdges env node = concatMap (getTypeEdges env) subs
  where
    subs = case node of
      H.ConDecl _ ts -> ts
      H.RecConDecl _ fields -> snd <$> fields

getTypeEdges :: HsEnv a -> H.Type -> [a]
getTypeEdges env node = case node of
  H.TyApp t ts -> concatMap (getTypeEdges env) $ t : ts
  H.TyCon name -> accTypeEnv env name
  H.TyLam ts t -> concatMap (getTypeEdges env) $ t : ts
