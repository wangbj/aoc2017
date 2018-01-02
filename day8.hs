{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

import qualified Data.ByteString.Char8 as C
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Read hiding (get, choice)
import qualified Text.ParserCombinators.ReadP as ReadP
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Identity
import           Numeric
import           Data.Maybe
import           Data.Either
import           Data.Semigroup hiding (option)

data Op = OpInc | OpDec deriving (Show, Eq)

instance Read Op where
  readsPrec _ = ReadP.readP_to_S readOp
    where readInc = ReadP.string "inc" >> return OpInc
          readDec = ReadP.string "dec" >> return OpDec
          readOp = ReadP.choice [readInc,  readDec]

data Cmp = Eq | Neq | Lt | Gt | Ge | Le deriving (Show, Eq)

eq = ReadP.string "==" >> return Eq
neq = ReadP.string "!=" >> return Neq
lt = ReadP.string "<" >> return Lt
gt = ReadP.string ">" >> return Gt
le = ReadP.string "<=" >> return Le
ge = ReadP.string ">=" >> return Ge

instance Read Cmp where
  readsPrec _ = ReadP.readP_to_S (ReadP.choice [eq, neq, lt, gt, le, ge])
  
register :: Monad m => ParsecT ByteString u m ByteString
register = C.pack <$> many1 letter

lexer :: Monad m => ParsecT ByteString u m ()
lexer = void (many space)

condIf :: Monad m => ParsecT ByteString u m ()
condIf = lexer >> void (string "if")

number :: Monad m => ParsecT ByteString u m Int
number = do
  lexer
  sign <- option '+' (char '-')
  dn   <- read <$> many1 digit
  return $ if sign == '+' then dn else -dn

op :: Monad m => ParsecT ByteString u m Op
op = do
  lexer
  t <- many1 letter
  return (read t)

type Reg = ByteString

type Context = (Max Int, HashMap Reg Int)

data Expr where
  IncIf :: Reg -> Int -> Cmp -> Reg -> Int -> Expr
  DecIf :: Reg -> Int -> Cmp -> Reg -> Int -> Expr

deriving instance Show Expr

doCmp :: Int -> Int -> Cmp -> Bool
doCmp v1 v2 op | op == Eq = v1 == v2
               | op == Neq = v1 /= v2
               | op == Lt = v1 < v2
               | op == Gt = v1 > v2
               | op == Le  = v1 <= v2
               | op == Ge  = v1 >= v2

expr :: Monad m => ParsecT ByteString u m Expr
expr = do
  r1 <- register
  lexer
  opr <- op
  n1 <- number
  condIf
  lexer
  r2 <- register
  lexer
  t <- many1 (noneOf " ")
  let cmp = read t
  n2 <- number
  return $! case opr of
    OpInc -> IncIf r1 n1 cmp r2 n2
    OpDec -> DecIf r1 n1 cmp r2 n2

eval :: MonadState Context m => Expr -> m ()
eval e@(IncIf reg val cmp reg2 val2) = do
  (o, env) <- get
  let val20 = fromMaybe 0 (HashMap.lookup reg2 env)
  when (doCmp val20 val2 cmp) $ do
    let env'= case HashMap.lookup reg env of
          Nothing -> HashMap.insert reg (0 + val) env
          Just x  -> HashMap.update (\old -> Just (old + val)) reg env
    put (o <> maybe mempty Max (HashMap.lookup reg env'), env')
  
eval e@(DecIf reg val cmp reg2 val2) = do
  (o, env) <- get
  let val20 = fromMaybe 0 (HashMap.lookup reg2 env)
  when (doCmp val20 val2 cmp) $ do
    let env' = case HashMap.lookup reg env of
          Nothing -> HashMap.insert reg (0 - val) env
          Just x  -> HashMap.update (\old -> Just (old - val)) reg env
    put (o <> maybe mempty Max (HashMap.lookup reg env'), env')

evalGetMax :: MonadState Context m => [Expr] -> m (Int, Int)
evalGetMax es = do
  mapM_ eval es
  (o, env) <- get
  return (getMax o, maximum (HashMap.elems env))

main = C.getContents >>= either print (\e -> print (evalState (evalGetMax e) (mempty, HashMap.empty))) . mapM (runIdentity . (\l -> runParserT expr 0 (C.unpack l) l)) . C.lines
