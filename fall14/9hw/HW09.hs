module HW09 where

import Control.Applicative
import Ring
import Test.QuickCheck
import Test.HUnit
import System.Random

-- exercise 1
instance Arbitrary Mod5 where
  arbitrary = arbitrary >>= return . MkMod

instance Arbitrary Mat2x2 where
  arbitrary = MkMat <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- exercise 2
-- haha nope maybe later.

-- exercise 3
-- associativity of add
prop_1 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_1 a b c = add (add a b) c == add a (add b c)

-- additive identity
prop_2 :: (Ring a, Eq a) => a -> Bool
prop_2 a = add addId a == a

-- additive inverse
prop_3 :: (Ring a, Eq a) => a -> Bool
prop_3 a = add a (addInv a) == addId

-- commutativity of addition
prop_4 :: (Ring a, Eq a) => a -> a -> Bool
prop_4 a b = add a b == add b a

-- associativity of mul
prop_5 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_5 a b c = mul (mul a b) c == mul a (mul b c)

-- mul id (1)
prop_6a :: (Ring a, Eq a) => a -> Bool
prop_6a a = mul mulId a == a

-- mul id (2)
prop_6b :: (Ring a, Eq a) => a -> Bool
prop_6b a = mul a mulId == a

-- left distributivity
prop_7 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_7 a b c = mul a (add b c) == add (mul a b) (mul a c)

-- right distributivity
prop_8 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_8 a b c = mul (add a b) c == add (mul a c) (mul b c)

-- exercise 4
-- this is gross, wish I had a way to only specify one argument...
prop_ring :: (Ring a, Eq a) => a -> a -> a -> Property
prop_ring a b c = singles .&&. doubles .&&. triples where
  singles = conjoin $ map ($ a) [prop_2, prop_3, prop_6a, prop_6b]
  doubles = prop_4 a b
  triples = conjoin . map ($ c) . map ($ b) $ map ($ a)
            [prop_1, prop_5, prop_7, prop_8]

-- exercise 5

-- The instance of Ring for Bool had (add) incorrectly defined as (||)
-- when it should be defined as (/=) - since we need True + True == False,
-- False + False == False, True + False = False + True == True.
-- Then we can have addInv == id .

-- Alternatively we can have add = (==) and addInv = not, but then
-- addId == True and True == mulId which makes our ring have the same
-- additive and multiplicative id's

-- Additionally the instance of Eq derived by Mod5 makes mkMod (-3) /=
-- mkMod 2.
instance Eq Mod5 where
  (==) (MkMod m) (MkMod n) = m `mod` 5 == n `mod` 5


-- exercise 6
data BST a = Leaf
           | Node (BST a) a (BST a)
  deriving Show

isBSTBetween :: Ord a => Maybe a -> Maybe a -> BST a -> Bool
isBSTBetween  _       _       Leaf = True
isBSTBetween m_lower m_upper (Node left x right)
  = isBSTBetween m_lower  (Just x) left  &&
    isBSTBetween (Just x) m_upper  right &&
    case m_lower of
      Just lower -> lower <= x
      Nothing    -> True
    &&
    case m_upper of
      Just upper -> x <= upper
      Nothing    -> True

isBST :: Ord a => BST a -> Bool
isBST = isBSTBetween Nothing Nothing

-- exercise 7
instance (Arbitrary a, Ord a, Random a) => Arbitrary (BST a) where
  arbitrary = do
    bound1 <- arbitrary
    bound2 <- suchThat arbitrary (> bound1)
    genBST bound1 bound2

genBST :: (Arbitrary a, Ord a, Random a) => a -> a -> Gen (BST a)
genBST lower upper
  | lower == upper = return Leaf
  | otherwise = do
      x <- choose (lower, upper)
      frequency [ (1, return Leaf)
                , (3, do
                      left <- genBST lower x
                      right <- genBST x upper
                      return $ Node left x right)]


-- exercise 9
parserTests :: Test
parserTests = TestList [ "Integer parsing positive" ~:
                         parseAll "1" ~?= Just (1 :: Integer)
                       , "Integer parsing negative" ~:
                         parseAll "-1" ~?= Just ((-1) :: Integer)
                       , "Integer parsing non-integer" ~:
                         parseAll "a" ~?= (Nothing :: Maybe Integer)
                       , "Mod5 parsing positive"~:
                         parseAll "6" ~?= Just (MkMod 6)
                       , "Mod5 parsing negative" ~:
                         parseAll "-1" ~?= Just (MkMod 4)
                       , "Mod5 parsing non-integer" ~:
                         parseAll "5.4" ~?= (Nothing :: Maybe Mod5)
                       , "Mat2x2 parsing" ~:
                         parseAll "[[-1, 2][3, 4]]" ~?= Just (MkMat (-1) 2 3 4)
                       , "Mat2x2 3 elements should fail" ~:
                         parseAll "[[-1, 2][3]]" ~?= (Nothing :: Maybe Mat2x2)
                       , "Bool parsing" ~:
                         parseAll "True" ~?= Just True
                       , "Bool non-bool value should fail" ~:
                         parseAll "1" ~?= (Nothing :: Maybe Bool)
                        ]

