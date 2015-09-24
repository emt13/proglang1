import PA1Helper
import Debug.Trace

-- Beta reduction
beta :: Lexp -> Lexp
beta (Atom a) = (Atom a)
beta (Apply (Lambda (Atom a) b) c) = beta' b a c
beta (Apply a b) = (Apply (reduce a) (reduce b))
beta (Lambda a b) = (Lambda (reduce a) (reduce b))

beta' :: Lexp -> String -> Lexp -> Lexp
beta' (Atom a) b c
  | a == b = c
  | otherwise = (Atom a)
beta' (Apply a b) d e = (Apply (beta' a d e) (beta' b d e))
beta' (Lambda a b) d e = (Lambda (beta' a d e) (beta' b d e))

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
alpha' (Apply a b) c n         = (Apply (alpha' a c n) (alpha' b c n))

-- Reduce function
-- apply alpha renaming, then pass to reduce
doreduce :: Lexp -> Lexp
doreduce lexp = reduce (doalpha lexp)

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
