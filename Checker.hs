module Checker
  ( CType(..)
  , CExpr(..)
  , check
  ) where

import qualified Data.Char   as Char
import qualified Data.Map    as Map
import qualified Debug.Trace as Debug
import           Expr        (Expr (..), Pretty (pretty))

dbg x = Debug.trace ("\n" ++ show x ++ "\n") x

type CExpr = Expr CType

data CType
  = CVar String
  | CInt
  | CBool
  | CFn CType CType
  deriving (Eq, Ord)

instance Show CType where
  show (CVar n)            = "'" ++ n
  show CInt                = "Int"
  show CBool               = "Bool"
  show (CFn a@(CFn _ _) r) = "(" ++ show a ++ ")->" ++ show r
  show (CFn a r)           = show a ++ "->" ++ show r

instance Pretty CType where
  pretty _ t = ":" ++ show t

instance Pretty () where
  pretty _ () = ""

type CheckCtx = (Int, Map.Map String CType, Map.Map CType CType)

newtype CheckState a =
  CheckState
    { runCheck :: CheckCtx -> Either String (CheckCtx, a)
    }

instance Functor CheckState where
  fmap f (CheckState checker) =
    CheckState $ \ctx0 -> do
      (ctx1, a) <- checker ctx0
      pure (ctx1, f a)

instance Applicative CheckState where
  pure a = CheckState $ \ctx0 -> Right (ctx0, a)
  (CheckState checkerFn) <*> (CheckState checkerA) =
    CheckState $ \ctx0 -> do
      (ctx1, fn) <- checkerFn ctx0
      (ctx2, a) <- checkerA ctx1
      pure (ctx2, fn a)

instance Monad CheckState where
  (CheckState checker) >>= f =
    CheckState $ \ctx0 -> do
      (ctx1, a) <- checker ctx0
      let (CheckState checkerB) = f a
      checkerB ctx1

checkFail s = CheckState $ \ctx0 -> Left s

newVar :: CheckState CType
newVar =
  CheckState $ \(i, names, vars) ->
    pure ((i + 1, names, vars), CVar $ "t" ++ show i)

setTypeRef :: CType -> CType -> CheckState ()
setTypeRef a b =
  CheckState $ \(i, names, vars) -> pure ((i, names, Map.insert a b vars), ())

checkEqual :: CType -> CType -> String -> CheckState ()
checkEqual a b _
  | a == b = pure ()
checkEqual _ _ s = checkFail s

unify :: CType -> CType -> CheckState ()
unify a b
  | a == b = pure ()
unify (CFn aa ab) b@(CFn ba bb) = unify aa ba >> unify ab bb
unify a@(CVar _) b = setTypeRef a b
unify a b@(CVar _) = unify b a
unify a b = checkFail $ "Can not unify " ++ show a ++ " with " ++ show b

findRoot :: CType -> CheckState CType
findRoot t =
  CheckState $ \ctx0@(i, names, vars) ->
    case Map.lookup t vars of
      Nothing -> pure ((i, names, vars), t)
      Just t2 -> runCheck (findRoot t2) ctx0

type ExprCtx = Map.Map String CType

getBound :: ExprCtx -> String -> CheckState CType
getBound ctx x = do
  case Map.lookup x ctx of
    Nothing -> checkFail $ "Unbound identifier '" ++ x ++ "'"
    Just t  -> pure t

checkSt :: ExprCtx -> Expr () -> CheckState (CExpr, CType)
checkSt ctx (Id x ()) = do
  t <- getBound ctx x
  t' <- findRoot t
  pure (Id x t', t')
checkSt ctx (App e0 e1) = do
  (e0c, t0) <- checkSt ctx e0
  (e1c, t1) <- checkSt ctx e1
  t' <- newVar
  unify t0 (CFn t1 t')
  t'' <- findRoot t'
  pure (App e0c e1c, t'')
checkSt ctx (Abs (x, ()) e) = do
  t <- newVar
  let newCtx = Map.insert x t ctx
  (ec, t') <- checkSt newCtx e
  tRoot <- findRoot t
  pure (Abs (x, tRoot) ec, CFn tRoot t')
checkSt ctx (Let x e0 e1) = do
  t' <- newVar
  let newCtx = Map.insert x t' ctx
  (e0c, t) <- checkSt newCtx e0
  let newCtx' = Map.insert x t ctx
  (e1c, t1) <- checkSt newCtx' e1
  pure (Let x e0c e1c, t1)
checkSt ctx (Const i) = pure (Const i, CInt)
checkSt ctx (ConstBool b) = pure (ConstBool b, CBool)
checkSt ctx (If cond e0 e1) = do
  (condc, tc) <- checkSt ctx cond
  checkEqual tc CBool $ "Expected Bool in if condition but found " ++ show tc
  (e0c, t0) <- checkSt ctx e0
  (e1c, t1) <- checkSt ctx e1
  checkEqual t0 t1 $
    "Expected both branches of if to be of same type, but they were " ++
    show t0 ++ " and " ++ show t1 ++ " respectively"
  pure (If condc e0c e1c, t0)

replaceVars :: Map.Map CType CType -> CType -> CType
replaceVars vars t@(CVar _) =
  case Map.lookup t vars of
    Nothing -> t
    Just t' -> replaceVars vars t'
replaceVars vars (CFn t0 t1) = CFn (replaceVars vars t0) (replaceVars vars t1)
replaceVars _ t = t

annotate :: Map.Map CType CType -> CExpr -> CExpr
annotate vars = fmap (replaceVars vars)

boolOp = CFn CBool (CFn CBool CBool)

comparison = CFn CInt (CFn CInt CBool)

intOp = CFn CInt (CFn CInt CInt)

builtin :: ExprCtx
builtin =
  Map.fromList
    [ ("&&", boolOp)
    , ("||", boolOp)
    , ("<", comparison)
    , (">", comparison)
    , ("==", comparison)
    , ("+", intOp)
    , ("-", intOp)
    , ("*", intOp)
    , ("/", intOp)
    ]

check :: Expr () -> Either String (CExpr, CType)
check e = do
  ((_, _, vars), (expr, t)) <-
    runCheck (checkSt builtin e) (0, Map.empty, Map.empty)
  pure (annotate vars expr, t)
