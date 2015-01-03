module HW05 where

import Ring
import Parser
import Data.Maybe    ( listToMaybe )
import Data.List    ( transpose )

-- exercise 2 - modular arithmetic
data Mod5 = MkMod Integer
            deriving (Show, Eq)

instance Ring Mod5 where
    addId = MkMod 0
    addInv (MkMod x) = MkMod (5 - x)
    mulId = MkMod 1

    add (MkMod x) (MkMod y) = MkMod ((x + y) `mod` 5)
    mul (MkMod x) (MkMod y) = MkMod ((x * y) `mod` 5)

readsIntToMod5 :: [(Integer, String)] -> [(Mod5, String)]
readsIntToMod5 [] = []
readsIntToMod5 ((x, s):xs) = (MkMod x, s) : readsIntToMod5 xs

instance Parsable Mod5 where
    parse = listToMaybe . readsIntToMod5 . reads

-- we would like to check if parsing Mod5s returns the right things
modParsingWorks :: Bool
modParsingWorks = (parse "3" == Just (MkMod 3, "")) &&
                  (parseRing "1 + 2 * 5" == Just (MkMod 1)) &&
                  (addId == (MkMod 0))

-- exercise 3 - matrix arithmetic
addVecs :: Num a => [a] -> [a] -> [a]
addVecs vec1 vec2 = zipWith (\x y -> x + y) vec1 vec2

-- just add a couple vectors to make sure this works as expected
addVecsWorks :: Bool
addVecsWorks = (addVecs [1,2] [3,4] == [4,6]) &&
               (addVecs [1..10] [2,4..20] == [3,6..30]) &&
               (addVecs [1..10] [(-1), (-2)..(-10)] == replicate 10 0)

dotProduct :: Num a => [a] -> [a] -> a
dotProduct vec1 vec2 = sum . map (\(x,y) -> x * y) $ zip vec1 vec2

-- make sure dotProduct works as expected
dotProductWorks :: Bool
dotProductWorks = (dotProduct [1,2] [3,4] == 11) &&
                  (dotProduct [0,0] [5,6] == 0)

mulRowByColumns :: Num a => [a] -> [[a]] -> [a]
mulRowByColumns row cols = [dotProduct row col | col <- cols]


data Mat2x2 = MkMat [[Integer]]
              deriving (Show, Eq)

instance Ring Mat2x2 where
    addId = MkMat [[0,0],[0,0]]
    addInv (MkMat x) = MkMat (map (map negate) x)
    mulId = MkMat [[1,0],[1,0]]

    add (MkMat mat1) (MkMat mat2) = MkMat $ zipWith addVecs mat1 mat2
    mul (MkMat mat1) (MkMat mat2) = MkMat $ map (\r -> mulRowByColumns r mat2) mat1

-- matrix math check: multiplication works, addition works
readsToMatBridge :: [([[Integer]], String)] -> [(Mat2x2, String)]
readsToMatBridge = map (\(l, s) -> (MkMat l, s))

instance Parsable Mat2x2 where
    parse = listToMaybe . readsToMatBridge . reads

-- we would like to check if parsing Mod5s returns the right things
matParsingWorks :: Bool
matParsingWorks = (parse "[[1, 2],[3,4]]" == Just (MkMat [[1,2],[3,4]], "")) &&
                  (parseRing "[[1,1],[1,1]] + [[1,2],[3,4]] * [[1,0],[0,1]]" == Just (MkMat [[2,3],[4,5]])) &&
                  (addId == (MkMat [[0,0],[0,0]]))

-- exercise 4 - boolean arithmetic
-- data Z2 = MkZ2 Bool
--           deriving (Show, Eq)

instance Ring Bool where
    addId = False
    addInv = (False ==)
    mulId = True

    add x y = ((x && y) == False) && (x || y)
    mul = (&&)

instance Parsable Bool where
    parse = listToMaybe . reads

-- we would like to check if parsing bools returns the right things
boolParsingWorks :: Bool
boolParsingWorks = (parse "False" == Just (False, "")) &&
                   (parseRing "True + True * False" == Just True) &&
                   (addId == False)

-- exercise 5
distribute :: RingExpr a -> RingExpr a
-- actual distribution
distribute (Mul x (Add y z)) = Add (distribute (Mul x y)) (distribute (Mul x z))
distribute (Mul (Add x y) z) = Add (distribute (Mul x z)) (distribute (Mul y z))
-- recursion rules
distribute (Add x y) = Add (distribute x) (distribute y)
distribute (Mul x y) = Mul (distribute x) (distribute y)
-- primitives
distribute x = x

distributeMaybe :: Maybe (RingExpr a) -> Maybe (RingExpr a)
distributeMaybe (Just x) = Just (distribute x)
distributeMaybe Nothing = Nothing

distributeWorks :: Bool
distributeWorks = ((distributeMaybe (parseRing "(1+2) * (3 + 4)")::Maybe (RingExpr Integer)) ==
                   Just (Add (Add (Mul (Lit 1) (Lit 3)) (Mul (Lit 2) (Lit 3)))
                             (Add (Mul (Lit 1) (Lit 4)) (Mul (Lit 2) (Lit 4)))
                        )) &&
                  ((distributeMaybe (parseRing "2 * (2 * (1 + 3))")::Maybe (RingExpr Integer)) ==
                   Just (Add (Mul (Lit 2) (Mul (Lit 2) (Lit 1)))
                             (Mul (Lit 2) (Mul (Lit 2) (Lit 3)))
                        ))

-- -- exercise 7
-- traverseTreeAndApplyFunc:: RingExpr a -> (RingExpr a -> RingExpr a) -> RingExpr a
-- -- traverseTreeAndApplyFunc Nothing f = Nothing
-- traverseTreeAndApplyFunc (Add x y) f = 

-- distributeTreeFunc:: (RingExpr a) -> Maybe (RingExpr a)
-- distributeTreeFunc Nothig
