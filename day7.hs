import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as S
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import           Numeric
import           Text.Parsec
import           Data.Maybe
import           Data.Either
import           Control.Monad
import           Control.Monad.Identity

type Node  = ByteString
type LNode = (Node, Int)

data Connect = Leaf LNode
             | Connect LNode [Node]
             deriving Show

node :: Monad m => ParsecT ByteString u m Node
node = many space >> fmap C.pack (many1 lower)

weighted :: Monad m => ParsecT ByteString u m LNode
weighted = do
  name <- node
  many space
  digits <- between (char '(') (char ')') (many1 digit)
  return (name, maybe 0 fst (listToMaybe (readDec digits)))

arrow :: Monad m => ParsecT ByteString u m ()
arrow = spaces >> void (string "->")

connectsTo :: Monad m => ParsecT ByteString u m Connect
connectsTo = do
  from <- weighted
  arrow
  tos  <- nodes
  return (Connect from tos)

leaf :: Monad m => ParsecT ByteString u m Connect
leaf = Leaf <$> weighted

nodes :: Monad m => ParsecT ByteString u m [Node]
nodes = sepBy node (char ',')

connection :: Monad m => ParsecT ByteString u m Connect
connection = try connectsTo <|> leaf

parseConnection s = runIdentity . runParserT connection 0 (C.unpack s) $ s

getinputs = sequence . map parseConnection . C.lines

-- the bottom (source) has 0 in degrees.
degrees :: [Connect] -> HashMap Node (Int, Int)
degrees = foldl ins HashMap.empty
  where ins h (Leaf s) = h
        ins h (Connect (from,w0) tos) = foldl (\m t -> HashMap.insertWith (\(x1, _) (x, y) -> (x+x1, y)) t (1,0) m) h' tos
          where nt = length tos
                h' = HashMap.insertWith (\(_,y1) (x,y) -> (x,y+y1)) from (0,nt) h

bottom = HashMap.filter (\(din, dout) -> din == 0) . degrees

main = C.getContents >>= either print (print . bottom) . getinputs
