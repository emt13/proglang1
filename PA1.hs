import PA1Helper
import Debug.Trace

-- Haskell representation of lambda expression
-- In Lambda Lexp Lexp, the first Lexp should always be Atom String
-- data Lexp = Atom String | Lambda Lexp Lexp | Apply Lexp  Lexp 

-- Given a filename and function for reducing lambda expressions,
-- reduce all valid lambda expressions in the file and output results.
-- runProgram :: String -> (Lexp -> Lexp) -> IO()

-- This is the identity function for the Lexp datatype, which is
-- used to illustrate pattern matching with the datatype. "_" was
-- used since I did not need to use bound variable. For your code,
-- however, you can replace "_" with an actual variable name so you
-- can use the bound variable. The "@" allows you to retain a variable
-- that represents the entire structure, while pattern matching on
-- components of the structure.
id' :: Lexp -> Lexp
id' v@(Atom _) = v
id' lexp@(Lambda (Atom _) _) = lexp
id' lexp@(Apply _ _) = lexp 

-- Beta reduction
beta :: Lexp -> Lexp
beta lexp@(Atom _) = lexp
beta lexp@(Apply (Lambda a v@(Atom b)) c)
  | a == v    = c
  | otherwise = v
beta lexp@(Apply (Lambda a (Apply b c)) d)
  | (a == b) && (a == c)    = (Apply d d)
  | otherwise               = (Apply b c)
beta lexp@(Apply v@(Lambda a g@(Lambda b c)) d) = beta (Lambda b (rep_var c a d))   
beta lexp@(Apply a b) = (Apply (reduce a) (reduce b))
beta lexp@(Lambda a b) = (Lambda (reduce a) (reduce b))

-- replace function    input -> lexp to replace -> replacement lexp
rep_var :: Lexp -> Lexp -> Lexp -> Lexp
rep_var v@(Atom d) to_rep rep
  | v == to_rep = rep
  | otherwise = v
rep_var lexp@(Lambda a b) to_rep rep
  | b == to_rep = (Lambda a rep)
  | otherwise = (Lambda a (rep_var b to_rep rep))
rep_var lexp@(Apply a b) to_rep rep = (Apply (rep_var a to_rep rep) (rep_var b to_rep rep))

-- Eta conversion
eta :: Lexp -> Lexp
eta lexp@(Atom _) = lexp
eta lexp@(Lambda a (Apply b c))
  | a == b    = lexp
  | otherwise = b
eta lexp@(Apply a b)  = (Apply (reduce a) (reduce b))
eta lexp@(Lambda a b) = (Lambda (reduce a) (reduce b))


-- Alpha Renaming
doalpha :: Lexp -> Lexp
doalpha lexp = alpha lexp "" 0

alpha :: Lexp -> String -> Integer -> Lexp
alpha (Atom a) c n = (Atom a)
alpha (Lambda (Atom a) b) c n = (alpha' (Lambda (Atom n') (alpha b a (n+1))) a n')
  where n' = (a++(show (n+1)))
alpha (Apply a b) c n         = (Apply (alpha a c n) (alpha b c n)) 

-- Alpha Renaming helper function
-- alpha'(lambda expression,bound variable,new variable name)
alpha' :: Lexp -> String -> String -> Lexp
alpha' (Atom a) c n
  | a == c    = (Atom n)  -- if atom is equal to bound, rename
  | otherwise = (Atom a)  -- else keep atom
-- Recursively call alpha' until an atom is reached
alpha' (Lambda (Atom a) b) c n = (Lambda (Atom a) (alpha' b c n))
alpha' (Apply a b) c n =  (Apply (alpha' a c n) (alpha' b c n))

-- Reduce function
-- apply alpha renaming, then pass to reduce
doreduce :: Lexp -> Lexp
doreduce lexp = reduce (doalpha lexp)

-- reduce(input lambda expression, output lambda expression)
-- if it is different, pass the reduced expression to reduce
reduce :: Lexp -> Lexp
reduce lexp
  | eta lexp /= lexp  = reduce(eta lexp)
  | beta lexp /= lexp = eta (reduce(beta lexp))
  | otherwise         = lexp


-- Entry point of program
main = do
    putStrLn "Please enter a filename containing lambda expressions:"
    fileName <- getLine
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram fileName doreduce 
