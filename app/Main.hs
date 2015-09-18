module Main where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as M
import Control.Monad.Trans.Either

type Name = String

data Exp = Lit Integer
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving (Show)

data Value = IntVal Integer
           | FunVal Env Name Exp
           deriving (Show)
type Env = M.Map Name Value

eval0 :: Env -> Exp -> Value
eval0 _   (Lit integer) = IntVal integer
eval0 env (Var name) = fromJust (M.lookup name env)
eval0 env (Plus expr1 expr2) = let IntVal int1 = eval0 env expr1 -- pattern matching in a let is new
                                   IntVal int2 = eval0 env expr2
                               in IntVal (int1 + int2)
eval0 env (Abs name expr) = FunVal env name expr
eval0 env (App expr1 expr2) = let val1 = eval0 env expr1
                                  val2 = eval0 env expr2
                              in case val1 of
                                  FunVal env' name body -> eval0 (M.insert name val2 env') body


exampleExp :: Exp
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev

eval1 :: Env -> Exp -> Eval1 Value
eval1 _ (Lit int) = return $ IntVal int
eval1 env (Var name) = return . fromJust $ M.lookup name env
eval1 env (Plus expr1 expr2) = do IntVal int1 <- eval1 env expr1
                                  IntVal int2 <- eval1 env expr2
                                  return $ IntVal (int1 + int2)
eval1 env (Abs name expr) = return $ FunVal env name expr
eval1 env (App expr1 expr2) = do val1 <- eval1 env expr1
                                 val2 <- eval1 env expr2
                                 case val1 of
                                     FunVal env' name body ->
                                        eval1 (M.insert name val2 env') body

type Eval2 a = EitherT String Identity a

runEval2 :: Eval2 a  -> Either String a
runEval2 ev = runIdentity (runEitherT ev)

eval2a :: Env -> Exp -> Eval2 Value
eval2a _ (Lit int) = return $ IntVal int
eval2a env (Var name) = return . fromJust $ M.lookup name env
eval2a env (Plus expr1 expr2) = do IntVal i1 <- eval2a env expr1
                                   IntVal i2 <- eval2a env expr2
                                   return $ IntVal (i1 + i2)
eval2a env (Abs name expr) = return $ FunVal env name expr
eval2a env (App expr1 expr2) = do val1 <- eval2a env expr1
                                  val2 <- eval2a env expr2
                                  case val1 of
                                      FunVal env' name body ->
                                        eval2a (M.insert name val2 env') body


eval2b :: Env -> Exp -> Eval2 Value
eval2b _ (Lit int) = return $ IntVal int
eval2b env (Var name) = case M.lookup name env of
                            Nothing  -> throwError ("unbound variable: " ++ name)
                            Just val -> return val
eval2b env (Plus expr1 expr2) = do expr1' <- eval2b env expr1
                                   expr2' <- eval2b env expr2
                                   case (expr1', expr2') of
                                       (IntVal i1, IntVal i2) ->
                                            return $ IntVal (i1 + i2)
                                       _ -> throwError "type error in addition"
eval2b env (Abs name expr) = return $ FunVal env name expr
eval2b env (App expr1 expr2) = do val1 <- eval2b env expr1
                                  val2 <- eval2b env expr2
                                  case val1 of
                                      FunVal env' name body ->
                                        eval2b (M.insert name val2 env') body
                                      _ -> throwError "type error in application"



eval2c :: Env -> Exp -> Eval2 Value
eval2c _ (Lit int) = return $ IntVal int
eval2c env (Var name) = case M.lookup name env of
                          Nothing  -> throwError "name undefined"
                          Just val -> return val
eval2c env (Plus expr1 expr2) = do expr1' <- eval2c env expr1
                                   expr2' <- eval2c env expr2
                                   case (expr1', expr2') of
                                     (IntVal i1, IntVal i2) ->
                                          return $ IntVal (i1 + i2)
                                     _ -> throwError "type error"
eval2c env (Abs name expr) = return $ FunVal env name expr
eval2c env (App expr1 expr2) = do FunVal env' name body <- eval2c env expr1
                                  val2                  <- eval2c env expr2
                                  eval2c (M.insert name val2 env') body


eval2 :: Env -> Exp -> Eval2 Value
eval2 = eval2b

type Eval3 a = ReaderT Env (EitherT String Identity) a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity (runEitherT (runReaderT ev env))

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do env <- ask
                   case M.lookup n env of
                       Nothing  -> (throwError ("unbound variable: " ++  n))
                       Just val -> return val
eval3 (Plus e1 e2) = do e1' <- eval3 e1
                        e2' <- eval3 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) ->
                                return $ IntVal (i1 + i2)
                            _ -> throwError "type error in addition"
eval3 (Abs n e) =   do env <- ask
                       return $ FunVal env n e
eval3 (App e1 e2) = do val1 <- eval3 e1
                       val2 <- eval3 e2
                       case val1 of
                           FunVal env' n body ->
                            local (const (M.insert n val2 env')) (eval3 body)
                           _ -> throwError "typeError in application"

type Eval4 a = ReaderT Env (EitherT String (StateT Integer Identity)) a

-- peel the layers off the onion

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st ev = runIdentity (runStateT ( runEitherT (runReaderT ev env)) st)


tick :: (Num s, MonadState s m) => m ()
tick = do st <- get
          put (st + 1)


eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = do tick
                   return $ IntVal i
eval4 (Var n) = do tick
                   env <- ask
                   case M.lookup n env of
                     Nothing  -> (throwError ("unbound variable: " ++  n))
                     Just val -> return val
eval4 (Plus e1 e2) = do tick
                        e1' <- eval4 e1
                        e2' <- eval4 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) ->
                              return $ IntVal (i1 + i2)
                          _ -> throwError "type error in addition"
eval4 (Abs n e) = do tick
                     env <- ask
                     return $ FunVal env n e
eval4 (App e1 e2) = do tick
                       val1 <- eval4 e1
                       val2 <- eval4 e2
                       case val1 of
                        FunVal env' n body ->
                          local (const (M.insert n val2 env')) (eval4 body)
                        _ -> throwError "typeError in application"


type Eval4' a = ReaderT Env (StateT Integer (EitherT String Identity)) a
-- instead of returning a either an error or the result and a state,
-- this one would return a state and either an error or the result

-- The above comment is exactly the opposite
-- Why did I mix it up? When reading the type signature of the second one,
-- it looks like The either is being computed before the state
-- but remember the onion, those transformers will be run from left to right,
-- so the state gets tested before the either and is subsumed by it
-- when in doubt, look at the type signature of the composite run function for
-- a good interpretation of the transformers

runEval4' :: Env -> Integer -> Eval4' a -> (Either String (a, Integer))
runEval4' env st ev = runIdentity (runEitherT (runStateT (runReaderT ev env) st))

-- use this relationship to control which level of the transformer stack handles errors

--doing the runX function at level x gives you the terminal X, wrapped up in the monad/transformer at the level below
-- runEitherT  on EitherT String (WriiterT [String] (StateT IntegerIdentity)) a
-- gives you WriterT [String] (StateT Integer Identity) (Either String a)
-- runWriterT on the above
-- gives you StateT Integer Identity (Either String a, [String])
-- runStateT on the above
-- gives you Identity ((Either String a, [String] ), Integer)

type Eval5 a = ReaderT Env (EitherT String (WriterT [String] (StateT Integer Identity))) a

runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st ev = runIdentity (runStateT (runWriterT (runEitherT (runReaderT ev env))) st)

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = do tick
                   return $ IntVal i
eval5 (Var n) = do tick
                   tell [n]
                   env <- ask
                   case M.lookup n env of
                     Nothing  -> (throwError ("unbound variable: " ++  n))
                     Just val -> return val
eval5 (Plus e1 e2) = do tick
                        e1' <- eval5 e1
                        e2' <- eval5 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) ->
                              return $ IntVal (i1 + i2)
                          _ -> throwError "type error in addition"
eval5 (Abs n e) = do tick
                     env <- ask
                     return $ FunVal env n e
eval5 (App e1 e2) = do tick
                       val1 <- eval5 e1
                       val2 <- eval5 e2
                       case val1 of
                        FunVal env' n body ->
                          local (const (M.insert n val2 env')) (eval5 body)
                        _ -> throwError "typeError in application"

type Eval6 a = ReaderT Env (EitherT String (WriterT [String] (StateT Integer IO))) a

runEval6 :: Env -> Integer -> Eval6 a -> IO ((Either String a, [String]),Integer)
runEval6 env st ev = runStateT (runWriterT (runEitherT (runReaderT ev env))) st


eval6 :: Exp -> Eval6 Value
eval6 (Lit i) = do tick
                   liftIO $ print i
                   return $ IntVal i
eval6 (Var n) = do tick
                   tell [n]
                   env <- ask
                   case M.lookup n env of
                     Nothing  -> (throwError ("unbound variable: " ++  n))
                     Just val -> return val
eval6 (Plus e1 e2) = do tick
                        e1' <- eval6 e1
                        e2' <- eval6 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) ->
                              return $ IntVal (i1 + i2)
                          _ -> throwError "type error in addition"
eval6 (Abs n e) = do tick
                     env <- ask
                     return $ FunVal env n e
eval6 (App e1 e2) = do tick
                       val1 <- eval6 e1
                       val2 <- eval6 e2
                       case val1 of
                        FunVal env' n body ->
                          local (const (M.insert n val2 env')) (eval6 body)
                        _ -> throwError "typeError in application"


main :: IO ()
main = undefined
