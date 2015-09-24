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


red :: Lexp -> Lexp
--red lexp@(Lambda e m) = trace ("L e: " ++ (show e) ++ " x: " ++ (show m)) beta ((red e) (red m))
--red lexp@(Apply e m) = trace ("A e: " ++ (show e) ++ " x: " ++ (show m)) beta ((red e) (red m))
red lexp@(Lambda e m) = trace ("L e: " ++ (show e) ++ " x: " ++ (show m)) beta lexp
red lexp@(Apply e m) = trace ("A e: " ++ (show e) ++ " x: " ++ (show m)) beta lexp
red v@(Atom _) = v

beta :: Lexp -> Lexp
beta (Apply (Lambda a b) c) 
  | a == b = c
  | otherwise = b
  
  
 

-- Entry point of program
main = do
    putStrLn "Please enter a filename containing lambda expressions:"
    fileName <- getLine
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram fileName red 
