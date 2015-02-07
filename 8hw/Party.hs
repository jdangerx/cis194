module Party where

import Data.Monoid
import Data.Tree
import Employee

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp { empFun = fun }) (GL ls funTot) = GL (emp:ls) (fun + funTot)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL ls1 f1) (GL ls2 f2) = GL (ls1 ++ ls2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f1 >= f2 = gl1
  | otherwise = gl2

getFun :: GuestList -> Fun
getFun (GL _ fun) = fun

-- Exercise 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold func (Node root subtrees) = func root (map (treeFold func) subtrees)

-- Exercise 3

-- If you invite the boss, you invite the combined best guest lists
-- that don't include immediate subordinates. If you don't invite the
-- boss, you invite the combined best guest lists.
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss subtrees = (withBoss, withoutBoss)
  where
    withBoss = glCons boss (foldr mappend mempty subListsWithBoss)
    withoutBoss = foldr mappend mempty subListsWithoutBoss
    subListsWithBoss = map snd subtrees
    subListsWithoutBoss = map (\(gl1, gl2) -> moreFun gl1 gl2) subtrees

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun tree = moreFun withCEO withoutCEO
  where
    gls = (treeFold nextLevel tree)
    withCEO = fst gls
    withoutCEO = snd gls


-- Exercise 5
getNames :: GuestList -> [String]
getNames (GL ls fun) = ["Total fun: " ++ show fun] ++ map empName ls

main :: IO ()
main = do
  fdata <- readFile "company.txt"
  let company = read fdata
  let guestList = maxFun company
  let guestListStr = getNames guestList
  mapM_ putStrLn guestListStr
