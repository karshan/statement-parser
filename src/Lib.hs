{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( parseAndEncode
    ) where

import qualified Data.Text as T
import Data.Char (digitToInt)
import Text.Parsec hiding (spaces)
import Text.Parsec.Text
import Text.Parsec.Char hiding (spaces)
import qualified Text.Parsec.Char as TPC (spaces)
import Protolude hiding (try, many, (<|>))
import Prelude (words, error, String)
import System.IO.Unsafe
import Data.Aeson
import           Data.Aeson.TH                 (defaultOptions, deriveJSON)
import           Data.Time                     (Day, UTCTime (..))
import           Data.Time.Format              (defaultTimeLocale, parseTimeM)
import Data.List (intercalate)

spaces = skipMany (oneOf " \t")

spacesOrNewlines :: Parser ()
spacesOrNewlines = TPC.spaces

line :: Parser String
line = many (noneOf "\n") <* newline

twoDigitNum :: Parser Int
twoDigitNum = do
    d1 <- digitToInt <$> digit
    d2 <- digitToInt <$> digit
    return $ (10 * d1) + d2

threeDigitNum :: Parser Int
threeDigitNum = do
    d1 <- digitToInt <$> digit
    d2 <- digitToInt <$> digit
    d3 <- digitToInt <$> digit
    return $ (100 * d1) + (10 * d2) + d3

type Date = String
type Description = String
type Amount = Int

data Txn = Txn { _date :: Date, _desc :: Description, _amt :: Amount } deriving (Eq, Show, Ord)
$(deriveJSON defaultOptions ''Txn)

twoDigitNumStr :: Parser String
twoDigitNumStr =
    (\x y -> [x, y]) <$> digit <*> digit

mdDateP :: Parser String
mdDateP = do
    month <- twoDigitNumStr
    char '/'
    day <- twoDigitNumStr
    return $ month ++ "/" ++ day

dateP :: Parser Date
dateP = do
    month <- twoDigitNumStr
    char '/'
    day <- twoDigitNumStr
    char '/'
    year <- twoDigitNumStr
    return $ month ++ "/" ++ day ++ "/" ++ year

oneToThreeDigitNum :: Parser Int
oneToThreeDigitNum = foldl (\a e -> (a * 10) + e) 0 <$> scan 3
    where
        scan :: Int -> Parser [Int]
        scan 0 = return []
        scan 3 = do
            x <- digitToInt <$> digit
            xs <- scan 2
            return (x:xs)
        scan n =
            try $ do
                x <- digitToInt <$> digit
                xs <- scan (n - 1)
                return (x:xs)
            <|>
            return []

amountP :: Parser Amount
amountP = do
    negM <- optionMaybe (char '-')
    dollarSignM <- optionMaybe (char '$')
    n1 <- oneToThreeDigitNum
    num <- try (char ',' *> (foldl (\a e -> (a * 1000) + e) n1 <$> sepBy1 threeDigitNum (char ',')))
            <|> return n1
    char '.'
    decimal <- twoDigitNum
    let amt = num * 100 + decimal
    return $ maybe amt (const (-amt)) negM

zeroToThreeDigitNum :: Parser Int
zeroToThreeDigitNum = foldl (\a e -> (a * 10) + e) 0 <$> scan 3
    where
        scan :: Int -> Parser [Int]
        scan 0 = return []
        scan n =
            try $ do
                x <- digitToInt <$> digit
                xs <- scan (n - 1)
                return (x:xs)
            <|>
            return []

-- Chase encode 0.25 as .25
chaseAmountP :: Parser Amount
chaseAmountP = do
    negM <- optionMaybe (char '-')
    dollarSignM <- optionMaybe (char '$')
    n1 <- zeroToThreeDigitNum
    num <- try (char ',' *> (foldl (\a e -> (a * 1000) + e) n1 <$> sepBy1 threeDigitNum (char ',')))
            <|> return n1
    char '.'
    decimal <- twoDigitNum
    let amt = num * 100 + decimal
    return $ maybe amt (const (-amt)) negM

manyTill' :: Parser a -> Parser end -> Parser ([a], end)
manyTill' p end = scan
    where
        scan = do { e <- end; return ([], e) }
             <|>
               do{ x <- p; (xs,e) <- scan; return ((x:xs), e) }

-- Don't use this to parse statments, it will miss 50% of checks
-- since 2 check transactions occur on the same line.
-- use bofaDebitTxnAndMaybeCheck
bofaDebitTxn :: Parser Txn
bofaDebitTxn = do
    date <- dateP
    spaces
    (desc, amount) <- manyTill' anyChar (try (skipMany1 (oneOf " \t") *> amountP))
    spacesOrNewlines
    return $ Txn date desc amount

bofaDebitTxnAndMaybeCheck :: Parser [Txn]
bofaDebitTxnAndMaybeCheck = do
    t1 <- bofaDebitTxn
    mT2 <- try (Just <$> bofaDebitTxn) <|> return Nothing
    return $ maybe [t1] (\t2 -> [t1, t2]) mT2

-- Remember credit card statments for January include transactions from the previous year
-- So this won't work correctly unless you preprocess input
bofaCreditTxn :: String -> Parser Txn
bofaCreditTxn year = do
    spaces
    txnDate <- mdDateP
    spaces
    postingDate <- mdDateP
    spaces
    (desc, amount) <- manyTill' anyChar (try (skipMany1 (oneOf " \t") *> amountP))
    return $ Txn (txnDate ++ "/" ++ year) (intercalate " " (words desc)) amount

chaseCreditTxn :: String -> Parser Txn
chaseCreditTxn year = do
    spaces
    txnDate <- mdDateP
    spaces
    (desc, amount) <- manyTill' anyChar (try (skipMany1 (oneOf " \t") *> chaseAmountP))
    eof
    return $ Txn (txnDate ++ "/" ++ year) (intercalate " " (words desc)) amount

parseTxns :: String -> String -> IO [Txn]
parseTxns f year = do
    s <- readFile f
    return $ rights $ map (parse (chaseCreditTxn year) "_") (T.lines s)
    --return $ rights $ map (parse (bofaCreditTxn year) "_") (T.lines s)

parseAndEncode :: String -> String -> IO ()
parseAndEncode f year = do
    txns <- parseTxns f year
    putStrLn $ encode $ 
        (if True then
            sortBy (compare `on` (fromMaybe undefined . fmap utctDay . parseTimeM True defaultTimeLocale "%m/%d/%Y" . _date))
        else
            identity)
        txns
