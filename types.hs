type String = [Char]

type Pos = (Int, Int)

type Trans = Pos -> Pos -- types must be recursive in haskell

--data is more powerful

data Move = North | South | East | West

a :: Pos
a = (1, 2)

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y -1)
move East (x, y) = (x + 1, y)
move West (x, y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves ms p = foldl (flip move) p ms

data Shape = Circle Float | Rect Float Float -- circle and rect are constructor functions

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r ^ 2 -- kind of like destructuring in JS
area (Rect x y) = x * y

data Maybe a = Nothing | Just a -- generalised / parameterised

data NaturalNumberBiggerZero = NaturalNumberBiggerZero Int deriving (Show)

d = NaturalNumberBiggerZero 2

e = NaturalNumberBiggerZero (-1)

addNat :: NaturalNumberBiggerZero -> NaturalNumberBiggerZero -> Prelude.Maybe NaturalNumberBiggerZero
addNat _ (NaturalNumberBiggerZero 0) = Prelude.Nothing
addNat (NaturalNumberBiggerZero 0) _ = Prelude.Nothing
addNat (NaturalNumberBiggerZero a) (NaturalNumberBiggerZero b)
  | a > 0 && b > 0 = Prelude.Just $ NaturalNumberBiggerZero $ a + b
  | otherwise = Prelude.Nothing

data List a = Nil | Cons a (List a) -- recursive definition

data Prop = Const Prelude.Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop

exp :: Prop
exp = And (Var 'a') (Var 'b')

type Assoc k v = [(k, v)]

type Subst = Assoc Char Prelude.Bool

assignment :: Subst
assignment = [('a', Prelude.True), ('b', Prelude.True)]

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (And a b) = eval s a && eval s b
eval s (Imply a b) = not (eval s a) || eval s b
eval s (Not a) = not (eval s a)

-- abstract machine

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y

data Expr = Val Int | Add Expr Expr

data Op = EVAL Expr | ADD Int

type Cont = [Op]

eval1 :: Expr -> Cont -> Int
eval1 (Val n) c = exec c n
eval1 (Add x y) c = eval1 x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval1 y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)

