{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Day18 where
import           AocPrelude
import           Data.Char  (isDigit)
import           Data.List  (groupBy)
import           Prelude    ()


data Expr = Literal Int | BinaryOp Char Expr Expr deriving(Show)

unpackOperator "+" = Just '+'
unpackOperator "*" = Just '*'
unpackOperator _   = Nothing

unpackNumber numberStr
    | all isDigit (unpack numberStr) = Just number
    | otherwise = Nothing
    where
         Right (number, "") = decimal numberStr

-- parseExpression :: [Text] -> (Expr, [Text])
-- parseExpression ("(":rest) = (result, newRest)
--     where
--         (result, ")":newRest) = parseExpression rest
-- parseExpression ((unpackNumber -> Just number):(unpackOperator -> Just op):rest) = (BinaryOp op (Literal number) rhs, newRest)
--     where
--         (rhs, newRest)  = parseExpression rest
-- parseExpression ((unpackNumber -> Just number):rest) = (Literal number, rest)

parseArg :: [Text] -> (Expr, [Text])
parseArg ((unpackNumber -> Just number):rest) = (Literal number, rest)
parseArg (")":rest) = (result, newRest)
    where (result, "(":newRest) = parseExpression rest


parseExpression tokens = case rest of
    ((unpackOperator -> Just op): restAfterOp) -> (BinaryOp op lh rh, restAfterRh)
        where
            (rh, restAfterRh) = parseExpression restAfterOp

    other                                         -> (lh, other)
    where
        (lh, rest) = parseArg tokens


evalExpression (Literal x)          = x
evalExpression (BinaryOp '+' lh rh) = evalExpression lh + evalExpression rh
evalExpression (BinaryOp '*' lh rh) = evalExpression lh * evalExpression rh


-- >>> parseExpression ["1", "+", "2"]
-- (BinaryOp '+' (Literal 1) (Literal 2),[])

-- >>> parseExpression ["1", "+", "2", "*", "3"]
-- (BinaryOp '+' (Literal 1) (BinaryOp '*' (Literal 2) (Literal 3)),[])

-- >>> parseExpression ["1", "+", "(","2", "*", "3",")", "+", "(","4" ,"*" ,"(","5" ,"+" ,"6",")",")"]
-- (BinaryOp '+' (Literal 1) (BinaryOp '+' (BinaryOp '*' (Literal 2) (Literal 3)) (BinaryOp '*' (Literal 4) (BinaryOp '+' (Literal 5) (Literal 6)))),[])

-- >>> (evalExpression . fst . parseExpression) ["1", "+", "(","2", "*", "3",")", "+", "(","4" ,"*" ,"(","5" ,"+" ,"6",")",")"]
-- 51

shouldJoin lhChar rhChar = all isDigit [lhChar, rhChar]

tokenize = map pack . groupBy shouldJoin . filter (/= ' ') . unpack

-- >>> tokenize "((25 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
-- ["(","(","25","+","4","*","9",")","*","(","6","+","9","*","8","+","6",")","+","6",")","+","2","+","4","*","2"]

-- >>> (evalExpression . fst . parseExpression . reverse . tokenize) "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
-- 13632

evalLine = evalExpression . fst . parseExpression . reverse . tokenize

solution = sum . map evalLine . lines

-- >>> runSolution solution (RealInput "18")
-- 800602729153
