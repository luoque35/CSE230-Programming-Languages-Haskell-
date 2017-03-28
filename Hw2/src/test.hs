module Test1 where

import Data.Char
import Data.Functor
import Control.Monad

newtype Parser a = P (String -> [(a, String)])

bindP p1 fp2 = P $ \cs -> [(y, cs'') | (x, cs')  <- doParse p1 cs
                                     , (y, cs'') <- doParse (fp2 x) cs']
returnP x = P (\cs -> [(x, cs)])

instance Monad Parser where
  (>>=)  = bindP
  return = returnP
oneChar :: Parser Char

instance Applicative Parser

instance Functor Parser where
  fmap f p = do x <- p
                return (f x)

doParse (P p) s = p s
oneChar = P (\cs -> case cs of
               c:cs' -> [(c, cs')]
               _     -> [])
pairP ::  Parser a -> Parser b -> Parser (a, b)
pairP p1 p2 = P (\cs ->
  [((x,y), cs'') | (x, cs' ) <- doParse p1 cs,
                   (y, cs'') <- doParse p2 cs']
  )
sequen :: (Monad m) => [m a] -> m [a]
sequen []     = return []
sequen (a:as) = do {x <- a; xs <- sequen as; return (x:xs) }
manyChar :: Int -> [Parser Char]
manyChar x
 | x<=0 = []
 | True = oneChar:(manyChar (x-1))

satP ::  (Char -> Bool) -> Parser Char
satP p = do c <- oneChar
            if p c then return c else failP
failP = P (\_ -> [])
strP :: String -> Parser String

strP a
 | (length a) ==0 = return []
 | (length a) > 0 = do{ c <- sequen (manyChar (length a));
 			 if (c==a) then return c else failP;}
dogeP = strP "doge"
