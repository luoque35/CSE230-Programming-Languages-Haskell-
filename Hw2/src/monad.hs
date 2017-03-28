
-- Part 3: An Interpreter for WHILE
-- ================================

-- Next, you will use monads to build an evaluator for
-- a simple *WHILE* language. In this language, we will
-- represent different program variables as

import Control.Monad.State hiding (when)
import Data.Map hiding(foldl)

import Text.Parsec hiding (State, between)
import Text.Parsec.Combinator hiding (between)
import Text.Parsec.Char
import Text.Parsec.String


type Variable = String

-- Programs in the language are simply values of the type

data Statement =
   Assign Variable Expression          -- x = e
  | If Expression Statement Statement   -- if (e) {s1} else {s2}
  | While Expression Statement          -- while (e) {s}
  | Sequence Statement Statement        -- s1; s2
  | Skip                                -- no-op
  deriving (Show)

-- where expressions are variables, constants or
-- binary operators applied to sub-expressions

data Expression =
    Var Variable                        -- x
  | Val Value                           -- v
  | Op  Bop Expression Expression
  deriving (Show)

-- and binary operators are simply two-ary functions

data Bop =
    Plus     -- (+)  :: Int  -> Int  -> Int
  | Minus    -- (-)  :: Int  -> Int  -> Int
  | Times    -- (*)  :: Int  -> Int  -> Int
  | Divide   -- (/)  :: Int  -> Int  -> Int
  | Gt       -- (>)  :: Int -> Int -> Bool
  | Ge       -- (>=) :: Int -> Int -> Bool
  | Lt       -- (<)  :: Int -> Int -> Bool
  | Le       -- (<=) :: Int -> Int -> Bool
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)

-- We will represent the *store* i.e. the machine's memory, as an associative
-- map from `Variable` to `Value`

type Store = Map Variable Value

-- **Note:** we don't have exceptions (yet), so if a variable
-- is not found (eg because it is not initialized) simply return
-- the value `0`. In future assignments, we will add this as a
-- case where exceptions are thrown (the other case being type errors.)

-- We will use the standard library's `State`
-- [monad](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2)
-- to represent the world-transformer.
-- Intuitively, `State s a` is equivalent to the world-transformer
-- `s -> (a, s)`. See the above documentation for more details.
-- You can ignore the bits about `StateT` for now.

-- Expression Evaluator
-- --------------------

-- First, write a function

evalE :: Expression -> State Store Value
-- State (Map Variable Value) Value

getValFromMap :: Store -> Variable -> Value
getValFromMap map var =
    let result = (Data.Map.lookup var map) in
        case result of
            Just value -> value
            otherwise -> IntVal(0)

evalE (Var x)      = do{
                        s <- get;
                        return (getValFromMap s x);
                    }


evalE (Val v)      = do { return v;}
evalE (Op o e1 e2) = do {
                        v1 <- evalE(e1);
                        v2 <- evalE(e2);
                        let v = evalOp o v1 v2 in
                        return v;
                    }

-- that takes as input an expression and returns a world-transformer that
-- returns a value. Yes, right now, the transformer doesnt really transform
-- the world, but we will use the monad nevertheless as later, the world may
-- change, when we add exceptions and such.

-- **Hint:** The value `get` is of type `State Store Store`. Thus, to extract
-- the value of the "current store" in a variable `s` use `s <- get`.


evalOp :: Bop -> Value -> Value -> Value
evalOp Plus (IntVal i) (IntVal j) = IntVal (i+j)
evalOp Minus (IntVal i) (IntVal j) = IntVal (i-j)
evalOp Times (IntVal i) (IntVal j) = IntVal (i*j)
evalOp Divide (IntVal i) (IntVal j)
    | j == 0 = IntVal(0)
    | otherwise = IntVal (i `div` j)
evalOp Gt (IntVal i) (IntVal j) = BoolVal (i>j)
evalOp Ge (IntVal i) (IntVal j) = BoolVal (i>=j)
evalOp Lt (IntVal i) (IntVal j) = BoolVal (i<j)
evalOp Le (IntVal i) (IntVal j) = BoolVal (i<=j)


-- >


-- Statement Evaluator
-- -------------------

-- Next, write a function

evalS :: Statement -> State Store ()

-- that takes as input a statement and returns a world-transformer that
-- returns a unit. Here, the world-transformer should in fact update the input
-- store appropriately with the assignments executed in the course of
-- evaluating the `Statement`.

-- **Hint:** The value `put` is of type `Store -> State Store ()`.
-- Thus, to "update" the value of the store with the new store `s'`
-- do `put s'`.


evalS (Assign x e )    = do{
                          map <- get;
                          v <- evalE(e);
                          put (Data.Map.insert x v map);
                          return ();
                        }
evalS w@(While e s)    = do{
                            v <- evalE(e);
                            case v of
                                BoolVal True -> evalS(Sequence s w)
                                otherwise -> return ();
                        }
evalS Skip             = return ();
evalS (Sequence s1 s2) = do{
                            evalS(s1);
                            evalS(s2);
                        }
evalS (If e s1 s2)     = do{
                            v <- evalE(e);
                            case v of
                                BoolVal True -> evalS(s1);
                                BoolVal False -> evalS(s2);
                                otherwise -> return ();
                        }

-- In the `If` case, if `e` evaluates to a non-boolean value, just skip both
-- the branches. (We will convert it into a type error in the next homework.)
-- Finally, write a function
--evalS :: Statement -> State Store ()

execS :: Statement -> Store -> Store
execS stat store = execState ( evalS(stat)) store

run :: Statement -> IO ()
run stmt = do putStrLn "Output Store:"
              putStrLn $ show $ execS stmt empty

-- Here are a few "tests" that you can use to check your implementation.

w_test = (Sequence (Assign "X" (Op Plus (Op Minus (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3))) (Op Plus (Val (IntVal 1)) (Val (IntVal 3))))) (Sequence (Assign "Y" (Val (IntVal 0))) (While (Op Gt (Var "X") (Val (IntVal 0))) (Sequence (Assign "Y" (Op Plus (Var "Y") (Var "X"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1))))))))
w_test2 = (Assign "X"
              (Op Plus
                  (Op Minus
                      (Op Plus (Val (IntVal 1)) (Val (IntVal 2)))
                      (Val (IntVal 3))
                  )
                  (Op Divide (Val (IntVal 1)) (Val (IntVal 0)))
              )
          )

w_fact = (Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (While (Op Gt (Var "N") (Val (IntVal 0))) (Sequence (Assign "X" (Var "N")) (Sequence (Assign "Z" (Var "F")) (Sequence (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))) (Assign "N" (Op Minus (Var "N") (Val (IntVal 1))))))))))


-- Parsing Constants
-- -----------------

-- First, we will write parsers for the `Value` type

valueP :: Parser Value
valueP = intP <|> boolP

-- To do so, fill in the implementations of

intP :: Parser Value
intP =  liftM (IntVal . read) $ many1 digit


-- Next, define a parser that will accept a
-- particular string `s` as a given value `x`

constP :: String -> a -> Parser a
constP s x  = do {
                  string s;
                  return x;
              }



-- and use the above to define a parser for boolean values
-- where `"true"` and `"false"` should be parsed appropriately.

boolP :: Parser Value
boolP = constP "true" (BoolVal True) <|> constP "false" (BoolVal False)
-- boolP = do {
--          x <- many1 letter;
--          case x of
--              "true"   -> return (BoolVal True);
--              "false"  -> return (BoolVal False);
--              _        -> error "Error in boolP";
--        }


-- Continue to use the above to parse the binary operators

{-
data Bop =
    Plus     -- (+)  :: Int  -> Int  -> Int
  | Minus    -- (-)  :: Int  -> Int  -> Int
  | Times    -- (*)  :: Int  -> Int  -> Int
  | Divide   -- (/)  :: Int  -> Int  -> Int
  | Gt       -- (>)  :: Int -> Int -> Bool
  | Ge       -- (>=) :: Int -> Int -> Bool
  | Lt       -- (<)  :: Int -> Int -> Bool
  | Le       -- (<=) :: Int -> Int -> Bool
  deriving (Show)

toOp :: String -> Bop
toOp "+" = Plus
toOp "-" = Minus

opP :: Parser Bop
op = do{
        x <- many1 (oneOf "+-*/<=>")
        return (toOp x)
    }
-}
opP :: Parser Bop
opP =  constP "+" (Plus) <|> constP "-" (Minus)
          <|> constP "*" (Times) <|> constP "/" (Divide)
          <|> constP ">" (Gt) <|> constP "<" (Lt)
          <|> constP ">=" (Ge) <|> constP "<=" (Le)

-- Parsing Expressions
-- -------------------

-- Next, the following is a parser for variables, where each
-- variable is one-or-more uppercase letters.

varP :: Parser Variable
varP = many1 upper
{-
data Statement =
    Assign Variable Expression          -- x = e
  | If Expression Statement Statement   -- if (e) {s1} else {s2}
  | While Expression Statement          -- while (e) {s}
  | Sequence Statement Statement        -- s1; s2
  | Skip                                -- no-op
  deriving (Show)

-- where expressions are variables, constants or
-- binary operators applied to sub-expressions

data Expression =
    Var Variable                        -- x
  | Val Value                           -- v
  | Op  Bop Expression Expression
  deriving (Show)
-}
-- Use the above to write a parser for `Expression` values

valEP :: Parser Expression
valEP = do{
            x <- valueP;
            return (Val x);
        }
bracketEP :: Parser Expression
bracketEP = do{
            spaces >> char '(' >> spaces;
            e <- exprP;
            spaces >> char ')' >> spaces;
            return e;

        }
varEP :: Parser Expression
varEP = do{
            x <- varP;
            return (Var x);
        }

opEP :: Parser Expression
opEP = do{
            spaces;
            e1 <- baseP;
            spaces;
            opr <- opP;
            spaces;
            e2 <- exprP;
            spaces;
            return (Op opr e1 e2);
      }

baseP :: Parser Expression
baseP = try bracketEP <|> valEP <|> varEP

exprP :: Parser Expression
exprP = try opEP <|> baseP
-- Parsing Statements
-- ------------------

-- Next, use the expression parsers to build a statement parser

ifP :: Parser Statement
ifP = do{
          spaces >> string "if" >> spaces;
          cond <- exprP;
          spaces >> string "then" >> spaces;
          thenClause <- statementP;
          spaces >> string "else" >> spaces;
          elseClause <- statementP;
          spaces>> string "endif" >> spaces;
          return (If cond thenClause elseClause);
      }

skipP :: Parser Statement
skipP = constP "skip" Skip

whileP :: Parser Statement
whileP = do{
            spaces >> string "while" >> spaces;
            cond <- exprP;
            spaces >> string "do" >> spaces;
            stt <- statementP;
            spaces >> string "endwhile" >> spaces;
            return (While cond stt);
          }

errorMsgP :: String -> Parser Statement
errorMsgP msg = error msg

errorP :: Parser Statement
errorP = error "Here"

sequenceP :: Parser Statement
sequenceP = do {
              spaces;
              s1 <- assignP <|> whileP <|> ifP <|> skipP ;
              spaces >> char ';' >> spaces;
              s2 <- statementP;
              spaces;
              return (Sequence s1 s2);
            }

assignP :: Parser Statement
assignP = do{
              spaces;
              var <- varP;
              spaces >> string ":=" >> spaces;
              exp <- exprP;
              spaces;
              return (Assign var exp);
          }

statementP :: Parser Statement
statementP = try sequenceP <|> assignP <|> whileP <|> ifP <|> skipP

-- When you are done, we can put the parser and evaluator together
-- in the end-to-end interpreter func tion

runFile s = do p <- parseFromFile statementP s
               case p of
                 Left err   -> print err
                 Right stmt -> run stmt


-- When you are done you should see the following at the ghci prompt

-- ~~~~~{.haskell}
-- ghci> runFile "test.imp"
-- Output Store:
-- fromList [("X",IntVal 0),("Y",IntVal 10)]

-- ghci> runFile "fact.imp"
-- Output Store:
-- fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
-- ~~~~~
