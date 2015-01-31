{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import Data.List (sort)
import Data.Maybe (listToMaybe)

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- exercise 1
ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Array arr) = Array $ fmap ynToBool arr
ynToBool (Object obj) = Object $ fmap ynToBool obj
ynToBool val = val

-- exercise 2
parseData :: B.ByteString -> Either String Value
parseData = fmap ynToBool . eitherDecode

-- exercise 3
data Market = Market { marketname :: T.Text
                     , x :: Float
                     , y :: Float
                     , state :: T.Text
                     , vegetables :: Bool
                     , meat :: Bool
                     }
            deriving (Show, Generic)
instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
                -- B.ByteString -> Either String Value
                -- Either String Value -> Either String [Market]
parseMarkets = lastStep . fmap resultValToResultMarkets . fmap fromJSON . parseData

resultValToResultMarkets :: Result Value -> Result [Market]
resultValToResultMarkets (Error s) = Error s
resultValToResultMarkets (Success arr@(Array _)) = fromJSON arr
resultValToResultMarkets (Success _) = Error "No top-level list of markets!"

lastStep :: Either String (Result [Market]) -> Either String [Market]
lastStep (Left s) = Left s
lastStep (Right (Error s)) = Left s
lastStep (Right (Success v)) = Right v

-- exercise 4
loadData :: IO [Market]
loadData = do
  fileData <- B.readFile "markets.json"
  let markets = parseMarkets fileData
  unEither markets where
           unEither (Left s) = fail s
           unEither (Right v) = return v

-- exercise 5

data OrdList a = OrdList { getOrdList :: [a] }
               deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
    mempty = OrdList []
    mappend (OrdList l1) (OrdList l2) = OrdList (sort (l1 ++ l2))

evens :: OrdList Integer
evens = OrdList [2,4,6]

odds :: OrdList Integer
odds = OrdList [1,3,5]

combined :: OrdList Integer
combined = evens <> odds

isCombined :: Bool
isCombined = combined == OrdList [1,2,3,4,5,6]

-- exercise 6
type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
-- returns something that takes a text and a markets and spits out a monoid
search mk_m txt markets = go markets where
    go [] = mempty
    go (mkt@(Market {marketname = name}):mkts)
        | T.isInfixOf txt name = mk_m mkt <> go mkts
        | otherwise = go mkts

marketToList :: Market -> [T.Text]
marketToList (Market {marketname = name}) = [name]

testSearch :: IO [T.Text]
testSearch = do
  markets <- loadData
  return $ search marketToList "Kaiser" markets

-- exercise 7
compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.) . (.)

firstFound :: Searcher (Maybe Market)
firstFound = compose2 listToMaybe $ search (:[])

-- exercise 8
lastFound :: Searcher (Maybe Market)
lastFound = compose2 listToMaybe $ compose2 reverse (search (:[]))

-- exercise 9
allFound :: Searcher [Market]
allFound = search (:[])

-- exercise 10
numberFound :: Searcher Int
numberFound = compose2 length $ search (:[])

-- exercise 11
instance Eq Market where
    (==) (Market {y = long1}) (Market {y = long2}) = long1 == long2
instance Ord Market where
    compare (Market {y = long1}) (Market {y = long2}) = compare long1 long2

orderedNtoS :: Searcher [Market]
orderedNtoS = compose2 reverse $ compose2 sort allFound
