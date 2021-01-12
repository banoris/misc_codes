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


{-
https://www.youtube.com/watch?v=YaLR6VaoWrA
	`Staring at ($), (<$>), (<*>) and (>>=)`
	  https://odone.io/posts/2020-02-18-dollar-functor-applicative-monad.html

-}
t1 = (*5) <$>  [1,2,3] -- [5, 10, 15]
t2 = fmap (*5) [1,2,3] -- [5, 10, 15]
t3 = map (*5)  [1,2,3] -- [5, 10, 15]

r1 = fmap (1+) (Just 2)  -- (Just 3)
r2 = liftM (1+) (Just 2) -- (Just 3)
r3 = (1+) <$> (Just 2)   -- (Just 3)
