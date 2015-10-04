Evan Thompson, Michael Dewey

PA 1

Features:
Alpha renaming, beta reduction, eta reduction
 ex:
   - (\x.\y.(y x) (y w))
   - \x.(y x)
   - (((\b.\t.\e((b t) e) \x.\y.x) x) y)
   - etc. 

In order to do alpha renaming, we append a number onto the end of a variable name. In the case of (\x.\y.(y x) (y w)),
the answer would be \y2.(y2 (y w)) where y2 is the renamed variable. In the example in the homework write up, this is equivalent to the
'z' in that answer. The naming convention is arbitrary provided that it is consistent

run by:

ghci

:l PA1.hs

:main 
input.lambda.txt
