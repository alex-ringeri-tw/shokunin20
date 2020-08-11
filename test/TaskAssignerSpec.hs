module TaskAssignerSpec (spec) where 

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import TaskAssigner
import qualified Data.Set as Set
import qualified Data.MultiSet as MS

arbitraryPositiveTasks :: Gen [Task]
arbitraryPositiveTasks = listOf $ (\(Positive a) -> Task a) <$> arbitrary

arbitraryTasksWithBounds :: (Int, Int) -> Gen [Task]
arbitraryTasksWithBounds b = listOf  (Task <$> choose b)

spec :: Spec
spec = do
  describe "TaskAssigner.assignDFS'" 
    (commonAssignmentChecks assignDFS' 10) 
  describe "TaskAssigner.assignMemo" $ do
    commonAssignmentChecks assignMemo 50
    modifyArgs (\args -> args { maxSize = 150 }) $
      it "should handle large lists with low task values" $ 
        forAll 
          (arbitraryTasksWithBounds (1, 10))
          (checkEquallySplit assignMemo)
    it "should reject tasks with negative values" $
      assignMemo [Task $ -1] `shouldBe` Nothing
  describe "TaskAssigner.assignDFS" 
    (commonAssignmentChecks assignDFS 20)
  describe "TaskAssigner.assignNonMemo" 
    (commonAssignmentChecks assignNonMemo 20)

commonAssignmentChecks :: TaskAssigner -> Int -> Spec
commonAssignmentChecks f maxLength = do
  it "returns base case on empty list" $
    f [] `shouldBe` Just (TaskAssignment [] [] [])
  it "returns equally split tasks for example" $
    f (mkTasks [5, 4, 1, 2, 7, 8, 3]) 
      `shouldBeEquivalentTo` Just (TaskAssignment
        [ Task 1, Task 4, Task 5 ]
        [ Task 8, Task 2 ]
        [ Task 3, Task 7])
  it "returns equally split tasks" $ 
    forAll 
      (arbitraryPositiveTasks `suchThat` (( <= maxLength) . length))
      (checkEquallySplit f)
  it "returns nothing if tasks cannot be split equally" $
    f (mkTasks [1, 2, 3]) `shouldBe` Nothing

mkTasks :: [Int] -> [Task]
mkTasks = fmap Task

shouldBeEquivalentTo :: Maybe TaskAssignment -> Maybe TaskAssignment -> Expectation
shouldBeEquivalentTo a e = 
  asSet <$> a `shouldBe` asSet <$> e
  where 
    asSet r = Set.fromList [
                (MS.fromList . child1) r
                , (MS.fromList . child2) r
                , (MS.fromList . child3) r ] 

checkEquallySplit :: (Foldable t, Functor t) => (t Task -> Maybe TaskAssignment) -> t Task -> Property
checkEquallySplit f ts = 
  counterexample (show . length $ ts) $
  within 60000000 $
  case f ts of
        Just r -> sumPoints ts `divisibleBy` 3 
                    .&&. pointsAreEqual r
                    .&&. allPointsAssigned r
        _      -> discard

  where 
    pointsAreEqual r =     
           sumPoints (child1 r) === (sumPoints ts `div` 3)
      .&&. sumPoints (child2 r) === (sumPoints ts `div` 3)
      .&&. sumPoints (child3 r) === (sumPoints ts `div` 3)

    allPointsAssigned r = 
      length(child1 r) + length(child2 r) + length(child3 r) === length ts

divisibleBy :: Integral a => a -> a -> Bool
divisibleBy x y = 
  let (_, m) = x `divMod` y
  in m == 0

sumPoints :: (Foldable t, Functor t) => t Task -> Int
sumPoints c = sum $ points <$> c