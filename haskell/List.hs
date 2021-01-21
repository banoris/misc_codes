
----- Folding Practice -----
-- https://www.youtube.com/watch?v=0qvi_sTJbEw

myfoldr :: (a -> b -> b) -> b -> [a] -> b

-- 0 is the base case
sum = foldr (+) 0 [1,2,3,4,5]

-- NOTICE the order of operation
-- Think of accumulator as the result of current computation
--   This result is passed to the next computation
-- last_elem `op` base_case  is applied first!
-- (1) 5+0 = acc1
-- (2) 4+acc1 = acc2
-- (3) 3+acc2 = acc3
sum' = (1 + (2 + (3 + (4 + (5 + 0)))))


-- ## Reversing a list with foldl

rev = foldl (\acc x -> x:acc) [] [1,2,3,4,5]

acc1 = 1:[]
acc2 = 2:acc1
acc3 = 3:acc2
acc4 = 4:acc3
acc5 = 5:acc4

-- Notice that the cons operation is applied first to 1, the
-- first element in the list
rev' = (5:(4:(3:(2:(1:[])))))


-- ## What about foldM in monad? Does it makes sense that
-- it is left fold? Think about typical forloop in imperative lang
-- Most of the time you operate on the first elem to last elem

-- fold without base case

a1 = foldl (-) 0 [1,2,3] -- 0 - 1 - 2 - 3 = -6
a2 = foldl1 (-) [1,2,3]  -- 1 - 2 - 3     = -4

----- END Folding Practice -----
