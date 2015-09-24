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

beta :: Lexp -> Lexp
beta lexp@(Atom _) = lexp
beta lexp@(Apply (Lambda a b) c)
  | a == b    = c
  | otherwise = b
beta lexp@(Apply a b) = (Apply (reduce a) (reduce b))
beta lexp = lexp

eta :: Lexp -> Lexp
eta lexp@(Atom _) = lexp
eta lexp@(Lambda a (Apply b c))
  | a /= b    = b
  | otherwise = lexp
eta lexp@(Apply a b) = (Apply (reduce a) (reduce b))
eta lexp = lexp

reduce :: Lexp -> Lexp
reduce lexp
  | eta lexp /= lexp  = reduce (eta lexp)
  | beta lexp /= lexp = reduce (beta lexp)
  | otherwise         = lexp
  
  
 

-- Entry point of program
main = do
    putStrLn "Please enter a filename containing lambda expressions:"
    fileName <- getLine
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram fileName reduce 
