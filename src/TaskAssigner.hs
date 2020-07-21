module TaskAssigner (assignDFS, assignDFS', assignMemo, assignNonMemo, Task(..), TaskAssignment(..), TaskAssigner) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Array
import Data.Function (fix)

type TaskAssigner = [Task] -> Maybe TaskAssignment

newtype Task = Task
  { points :: Int
  } deriving (Show, Eq, Ord)

data TaskAssignment = TaskAssignment
  { child1 :: [Task]
  , child2 :: [Task]
  , child3 :: [Task]
  } deriving (Show, Eq)

assignDFS' :: TaskAssigner
assignDFS' = assign' (TaskAssignment [] [] [])

assign' :: TaskAssignment -> TaskAssigner
assign' r (t:ts) =
  assign' (assignTo child1' r) ts
    <|> assign' (assignTo child2' r) ts
    <|> assign' (assignTo child3' r) ts
  where
    assignTo c = c (t:)
assign' r [] 
  | sumPoints child1 == sumPoints child2 
      && sumPoints child2 == sumPoints child3 = Just r
  | otherwise = Nothing
  where 
    sumPoints f = sum $ points <$> f r
    
assignDFS :: TaskAssigner
assignDFS tasks = do
  guard (m == 0)
  case tasks of 
    (t:ts) -> cAssign' (TaskAssignment [t] [] []) ts
    _      -> return (TaskAssignment [] [] [])
  
  where
    s = sum $ points <$> tasks
    (tgt, m) = s `divMod` 3

    cAssign' r (t:ts) =
      assignTo' child1 child1'
        <|> assignTo' child2 child2'
        <|> assignTo' child3 child3' 
      where
        assignTo' c c' = do
          guard $ sumPoints c + points t <= tgt
          cAssign' (assignTo c' r) ts

        assignTo child = child (t:)
        sumPoints f = sum $ points <$> f r
    cAssign' r [] 
      | sumPoints child1 == sumPoints child2 
          && sumPoints child2 == sumPoints child3 = Just r
      | otherwise = Nothing
      where 
        sumPoints f = sum $ points <$> f r

assignNonMemo :: TaskAssigner
assignNonMemo ts = do
  let n = length ts
  let tasks = listArray (1, n) ts
  let s = sum $ points <$> tasks
  let (tgt, m) = s `divMod` 3
  guard (m == 0)

  let assignR = fix (canAssign tasks)

  assignR tgt tgt n
              

assignMemo :: TaskAssigner
assignMemo ts = do
  guard $ all (\t -> points t >= 0) ts
  let n = length ts
  let tasks = listArray (1, n) ts
  let s = sum $ points <$> tasks
  let (tgt, m) = s `divMod` 3
  guard (m == 0)

  let memoize = memo ((0,0,0), (tgt,tgt,n))
  let assign_' = canAssign tasks
  let assignMemo' = fix (memoize . assign_') 

  assignMemo' tgt tgt n

type Point3 = (Int, Int, Int)

memo :: (Point3, Point3) -> (Int -> Int -> Int -> a) -> Int -> Int -> Int -> a
memo b f = 
  let 
    ((i',j',k'), (i'',j'',k'')) = b
    a = array 
          b 
          [ ((i,j,k), f i j k) | i <- [i'..i'']
                               , j <- [j'..j'']
                               , k <- [k'..k''] ]
  in \i j k -> a ! (i, j, k)

canAssign :: Array Int Task 
          -> (Int -> Int -> Int -> Maybe TaskAssignment) 
          -> Int -> Int -> Int -> Maybe TaskAssignment
canAssign tasks _ 0 0 0 = return $ TaskAssignment [] [] (elems tasks)
canAssign _     _ _ _ 0 = Nothing
canAssign tasks _ 0 0 _ = return $ TaskAssignment [] [] (elems tasks)
canAssign tasks f i j k = 
        canAssignFirst  (assignTo child1')
    <|> canAssignSecond (assignTo child2')
    <|> f i j (k-1)
  where
    t = tasks ! k
    assignTo child = child (t:) . child3' (delete t) 

    canAssignFirst  = canAssign' i (\i' -> f i' j  (k-1))
    canAssignSecond = canAssign' j (\j' -> f i  j' (k-1))
    canAssign' v f' u = 
      let v' = v - points t
      in  when' (v' >= 0) (u <$> f' v')
    

delete :: Eq a => a -> [a] -> [a]
delete t (a:as) =
  if t == a then as
  else a : delete t as
delete _ [] = []

when' :: Bool -> Maybe a -> Maybe a
when' b m = if b then m else Nothing

type ModifyTask = ([Task] -> [Task]) -> TaskAssignment -> TaskAssignment

child1' :: ModifyTask
child1' f s = s { child1 = f $ child1 s }
child2' :: ModifyTask
child2' f s = s { child2 = f $ child2 s }
child3' :: ModifyTask
child3' f s = s { child3 = f $ child3 s }
