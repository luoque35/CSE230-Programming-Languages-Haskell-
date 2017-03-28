Part 3: An Interpreter for WHILE
================================

\begin{code}
{-@ LIQUID "--no-termination" @-}
{-# LANGUAGE  FlexibleContexts,FlexibleInstances #-}

module Interpreter (interpret) where
--import Control.Monad.State
import           Prelude hiding (lookup)
import qualified Data.Set as S

\end{code}

**HINT:** To do this problem, first go through [this case study](http://ucsd-progsys.github.io/lh-workshop/05-case-study-eval.html)
also included in your tarball as `case-study-eval.lhs`.

Next, you will revisit your interpreter for the *WHILE*
language to ensure that execution never fails due to a
*use-before-definition* error.

Programs in the language are simply values of the type

\begin{code}
data Statement =
    Assign Variable Expression            -- x = e
  | IfZ    Expression Statement Statement -- if (e) {s1} else {s2}
  | WhileZ Expression Statement           -- while (e) {s}
  | Sequence Statement Statement          -- s1; s2
  | Skip                                  -- no-op
\end{code}

to simplify matters, we assume the branch statement `IfZ e s1 s2`
evaluates `e` and then executes `s1` if the value of `e` equals `0`
and otherwise evaluates `s2`.

Thus, the expressions are variables, constants or binary operators applied to sub-expressions

\begin{code}
data Expression =
    Var Variable                        -- x
  | Val Value                           -- v
  | Op  Bop Expression Expression
\end{code}

and binary operators are simply two-ary functions

\begin{code}
data Bop =
    Plus     -- (+)  :: Int  -> Int  -> Int
  | Minus    -- (-)  :: Int  -> Int  -> Int
\end{code}

and variables and values are just:

\begin{code}
type Variable = String
type Value    = Int
\end{code}

Store
-----

We will represent the *store* i.e. the machine's memory, as a list of
`Variable` - `Value` pairs:

\begin{code}

type Store = [(Variable, Value)]

{-@ measure listElts2 :: Store -> [Variable]@-}
listElts2([])       = (S.empty)
listElts2((x,_):xs) = (S.union (S.singleton x) (listElts2 xs))

{-@ measure toVList :: Store -> [Variable] @-}
toVList :: Store -> [Variable]
toVList s = Prelude.map (\(k,v) -> k) s

{-@ update :: s: Store -> k:Variable -> Value -> Store @-}
update :: Store -> Variable -> Value -> Store
update st x v = (x, v) : st

{-@ lookup :: k:Variable -> {s:Store | S.member k (S.fromList (toVList s)) }-> Value @-}
lookup :: Variable -> Store -> Value
lookup x ((y, v) : st)
  | x == y         = v
  | otherwise      = lookup x st
lookup x []        = impossible "variable not found"
\end{code}

Evaluation
----------

We can now write a function that evaluates `Statement` in a `Store` to yield a
new *updated* `Store`:

\begin{code}

evalS :: Store -> Statement -> Store

evalS st Skip             = st

evalS st (Assign x e )    = update st x v
                            where
                              v = evalE st e

evalS st (IfZ e s1 s2)    = if v == 0
                              then evalS st s1
                              else evalS st s2
                            where
                              v = evalE st e
evalS st w@(WhileZ e s)   = if v == 0
                              then evalS st (Sequence s w)
                              else st
                            where
                              v = evalE st e

evalS st (Sequence s1 s2) = evalS (evalS st s1) s2
\end{code}

The above uses a helper that evaluates an `Expression` in a `Store` to get a
`Value`:

\begin{code}
evalE :: Store -> Expression -> Value
evalE st (Var x)      = lookup x st
evalE _  (Val v)      = v
evalE st (Op o e1 e2) = evalOp o (evalE st e1) (evalE st e2)

evalOp :: Bop -> Value -> Value -> Value
evalOp Plus  i j = i + j
evalOp Minus i j = i - j
\end{code}

GOAL: A Safe Evaluator
----------------------

Our goal is to write an evaluator that *never* fails due to an undefined
variable. This means, we must ensure that the evaluator is never called
with *malformed* programs in which some variable is *used-before-being-defined*.

In particular, this corresponds to establishing that the call to impossible
*never* happens at run time, by verifying that the below typechecks:

\begin{code}
{-@ impossible :: {v:String | false} -> a @-}
impossible msg = error msg
\end{code}

Obviously it is possible to throw an exception if you run `evalS` with a
*bad* statement. Your task is to complete the implementation of `isSafe`
and add suitable refinement type specifications, such that you can prove
that the following `interpret` function is safe:

\begin{code}

type SS = S.Set Variable

{-@ interpret :: {s: Statement | isSafe s} -> Maybe Store@-}
interpret :: Statement -> Maybe Store
interpret s
  | isSafe s  = Just (evalS [] s)  -- `s` does not use any vars before definition
  | otherwise = Nothing            -- `s` may use some var before definition

{-@ inline isSafe @-}
isSafe :: Statement -> Bool
isSafe s = (readS s) == S.empty
\end{code}

To implement `isSafe` you probably need to write a function that computes the
`Set` of variables that are ``read-before-definition" in a `Statement` (and
`Expression`):

\begin{code}


{-@ measure readS @-}
readS :: Statement -> S.Set Variable
readS (Assign x e)     = readE e
readS (IfZ e s1 s2)    = (readE e) `S.union` (readS s1) `S.union` (readS s2)
readS (WhileZ e s)     = (readE e) `S.union` (readS s)
readS (Sequence s1 s2) =
  let free1    = readS s1
      defined1 = definedS s1
      free2    = (readS s2) `S.difference` defined1
  in  free2
readS Skip             = S.empty

{-@ measure readE @-}
readE :: Expression -> S.Set Variable
readE (Var x)          = S.singleton x
readE (Val v)          = S.empty
readE (Op o e1 e2)     = (readE e1) `S.union` (readE e2)


{-@ measure definedS @-}
definedS :: Statement -> S.Set Variable
definedS (Assign x e)     = S.singleton x
definedS (IfZ e s1 s2)    = (definedS s1) `S.intersection` (definedS s2)
definedS (WhileZ e s)     = S.empty
definedS (Sequence s1 s2) = (definedS s1) `S.union` (definedS s2)
definedS   Skip             = S.empty

\end{code}


Here are some example programs:

\begin{code}

safeStatement1 =
    (Assign "X" (Val 5))
    `Sequence`
    (Assign "Y" (Var "X"))

safeStatement2 =
     (IfZ (Val 0)
      (Assign "Z" (Val 1))
      (Assign "Z" (Val 2)))
     `Sequence`
     (Assign "Y" (Var "Z"))

unsafeStatement1 =
    (Assign "X" (Val 5))
    `Sequence`
    (Assign "Y" (Var "Z"))

unsafeStatement2 =
     (WhileZ (Val 0) (Assign "Z" (Val 1)))
     `Sequence`
     (Assign "Y" (Var "Z"))
\end{code}

When you are done

1. `liquid Interpreter.lhs` should return `SAFE`.

2. You should get the following behavior in `ghci`.

~~~~~
*Interpreter> interpret safeStatement1
Just [("Y",5),("X",5)]
*Interpreter> interpret safeStatement2
Just [("Y",1),("Z",1)]
*Interpreter> interpret unsafeStatement1
Nothing
*Interpreter> interpret unsafeStatement2
Nothing
~~~~~

Note that the second step is important, you could implement `isSafe _ = False`
which just *rejects* every program, but that is not a very useful interpreter.
