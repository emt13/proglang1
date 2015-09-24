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

--       input -> to_rep -> rep 
replace :: Lexp -> Lexp -> Lexp -> Lexp
replace v@(Atom d) to_rep rep
  | v == to_rep = rep
  | otherwise = v
replace lexp@(Lambda a b) to_rep rep
  | b == to_rep = (Lambda a rep)
  | otherwise = (Lambda a (replace b to_rep rep))
replace lexp@(Apply a b) to_rep rep = (Apply (replace a to_rep rep) (replace b to_rep rep))

beta :: Lexp -> Lexp
beta lexp@(Atom _) = lexp
beta lexp@(Apply (Lambda a v@(Atom b)) c)
  | a == v    = c
  | otherwise = v
beta lexp@(Apply (Lambda a (Apply b c)) d)
  | (a == b) && (a == c)    = (Apply d d)
  | a == b                  = (Apply d c)
  | a == c                  = (Apply b d)
  | otherwise               = (Apply b c)
beta lexp@(Apply v@(Lambda a g@(Lambda b c)) d) = beta (Lambda b (replace c a d))   
beta lexp@(Apply a b) = (Apply (reduce a) (reduce b))
beta lexp@(Lambda a b) = (Lambda (reduce a) (reduce b))

eta :: Lexp -> Lexp
eta lexp@(Atom _) = lexp
eta lexp@(Lambda a (Apply b c))
  | a == b    = lexp
  | otherwise = b
eta lexp@(Apply a b) = (Apply (reduce a) (reduce b))
eta lexp@(Lambda a b) = (Lambda (reduce a) (reduce b))

reduce :: Lexp -> Lexp
reduce lexp
--  | trace (show lexp) False == True = lexp
  | eta lexp /= lexp  = reduce(eta lexp)
  | beta lexp /= lexp = eta (reduce(beta lexp))
  | otherwise         = lexp
  
doalpha :: Lexp -> Lexp
doalpha lexp = alpha lexp "" 0

alpha :: Lexp -> String -> Integer -> Lexp
alpha (Atom a) c n
  | (c == a) = (Atom (a++"1"))
  | otherwise = (Atom a)
alpha (Lambda v@(Atom a) b) c n = (Lambda (Atom (a++(show 1))) (alpha b a (n+1)))
alpha (Apply a b) c n         = (Apply (alpha a c n) b) 
  
 {-
alpha :: Lexp -> String -> Integer -> Lexp
alpha (Atom a) c n
  | (c == "") = (Atom (a++(show n)))
  | otherwise = (Atom a)
alpha (Lambda v@(Atom a) b) c n = (Lambda (Atom (a++(show (n+1)))) (alpha b a (n+1)))
alpha (Apply a b) c n         = (Apply (alpha a "" n) (alpha b "" n)) 
  
-} 
 

-- Entry point of program
main = do
    putStrLn "Please enter a filename containing lambda expressions:"
    fileName <- getLine
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram fileName doalpha 
