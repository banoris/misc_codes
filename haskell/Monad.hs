import Control.Monad

{-
do
    a <- act         same as    act >>= \a -> rest
    rest

do act               same as    act
-}
take10 = do
    filename <- getLine
    contents <- readFile filename
    putStr (take 10 contents)

take10' = getLine >>= \filename -> readFile filename >>= \contents -> putStr (take 10 contents)

take10'' = getLine >>= readFile >>= putStr . take 10
