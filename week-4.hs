import Data.List

-- 1.1

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False 

-- >>> isJust (Just 5)
-- True

-- >>> isJust Nothing
-- False

-- 1.2

fromMaybe :: a -> Maybe a -> a
fromMaybe m (Just n) = n
fromMaybe m _ = m

-- >>> fromMaybe 10 (Just 5)
-- 5

-- >>> fromMaybe 10 Nothing
-- 10

-- 1.3

forget :: Either String a -> Maybe a
forget result = case result of
    Left str -> Nothing
    Right val -> Just val

-- >>> forget (Left "hi")
-- Nothing

-- >>> forget (Right 5)
-- Just 5

-- 1.4

-- Just (Right True) :: Maybe (Either False True)

-- 1.5a

safeDiv :: Int -> Int -> Either String Int
safeDiv _ 0 = Left "Cannot divide by zero!"
safeDiv x y = Right (x `div` y)

-- >>> safeDiv 5 0
-- Left "Cannot divide by zero!"

-- >>> safeDiv 4 2
-- Right 2

-- 1.5b

safeDiv' :: Int -> Int -> Maybe Int
safeDiv' _ 0 = Nothing
safeDiv' x y = Just (x `div` y)

-- >>> safeDiv' 5 0
-- Nothing

-- >>> safeDiv' 4 2
-- Just 2

-- 1.5c

checkForErrors :: [Either String a] -> Either String [a]
checkForErrors [] = Right []
checkForErrors (x:xs) = case x of
    Left errorMsg -> Left errorMsg
    Right n -> case checkForErrors xs of
        Left errorMsg -> Left errorMsg
        Right ns -> Right (n : ns)

-- >>> checkForErrors [Right 10, Right 9]
-- Right [10,9]

-- >>> checkForErrors [Right 10, Left "Error!", Right 9]
-- Left "Error!"

-- >>> checkForErrors [Right 10, Left "First error!", Right 9, Left "Second error!"]
-- Left "First error!"

-- 2.1

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Sub Expr Expr

size :: Expr -> Int
size (Lit n) = 0
size (Add x y) = 1 + size x + size y
size (Mul x y) = 1 + size x + size y
size (Sub x y) = 1 + size x + size y

-- >>> size (Add (Mul (Lit 3) (Lit 1)) (Lit 2))
-- 2

eval :: Expr -> Int
eval (Lit n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
eval (Sub x y) = eval x - eval y

-- >>> eval (Sub (Add (Mul (Lit 3) (Lit 1)) (Lit 2)) (Lit 2))
-- 3

pretty :: Expr -> String
pretty (Lit n) = show n 
pretty (Add x y) = pretty x ++ "+" ++ pretty y
pretty (Mul x y) = pretty x ++ "*" ++ pretty y
pretty (Sub x y) = pretty x ++ "-" ++ pretty y

-- >>> pretty (Sub (Add (Mul (Lit 3) (Lit 1)) (Lit 2)) (Lit 2))
-- "3*1+2-2"

-- 3.1

addTwo :: Num a => a -> a
addTwo x = x + 2

-- >>> addTwo (3 :: Int)
-- 5

-- >>> addTwo (4.7 :: Double)
-- 6.7 (lol)

-- >>> addTwo "h"
-- No instance for `Num String' arising from a use of `addTwo'
-- In the expression: addTwo "h"
-- In an equation for `it_a2cmf': it_a2cmf = addTwo "h"

-- 3.2a

data RPS = Rock | Paper | Scissors

-- 3.2b

instance Eq RPS where
    Rock == Rock = True
    Paper == Paper = True
    Scissors == Scissors = True 
    _ == _ = False

-- >>> Rock == Rock
-- True

-- >>> Scissors == Paper
-- False

-- 3.2c

data RPS' = Rock' | Paper' | Scissors' deriving Eq

-- >>> Scissors == Rock
-- False

-- 3.2d

shoot :: RPS -> RPS -> Bool
shoot Paper Rock = True
shoot Rock Scissors = True
shoot Scissors Paper = True
shoot _ _ = False

-- >>> shoot Rock Paper
-- False

-- >>> shoot Scissors Paper
-- True

instance Ord RPS where
    Rock < Paper = True
    Paper < Scissors = True
    Scissors < Rock = True
    _ < _ = False 

stab :: RPS -> RPS -> Bool
stab a b = a < b

-- >>> stab Rock Paper
-- True

-- >>> stab Paper Rock
-- False

-- 4.1

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq, Ord)

data Court = Ace Suit | Jack Suit | Queen Suit | King Suit deriving (Show, Eq, Ord)

type Pip = Int

type Rank = Either Pip Court

data Card = Joker | Card Suit Rank deriving Show

-- 4.2

instance Eq Card where
    Joker == Joker = True
    (Card a b) == (Card c d) = if (a == c) && (b == d) then True else False
    _ == _ = False

-- >>> (Card Hearts (Left 5)) == (Card Hearts (Left 5))
-- True

-- >>> (Card Hearts (Left 5)) == (Card Diamonds (Right (Ace Diamonds)))
-- False

-- 4.3

snap :: Card -> Card -> String
snap (Card _ n) (Card _ m)
    | n == m = "SNAP"
    | otherwise = "..."

-- >>> snap (Card Hearts (Left 5))  (Card Diamonds (Left 5))
-- "SNAP"

-- >>> snap (Card Hearts (Left 5)) (Card Hearts (Left 3))
-- "..."

-- 4.4


instance Ord Card where
    compare Joker Joker = EQ
    compare Joker _ = GT
    compare _ Joker = LT

    compare (Card s1 r1) (Card s2 r2)
        | s1 /= s2 = compare (suitOrder s1) (suitOrder s2)
        | otherwise = compare (rankOrder r1) (rankOrder r2)

suitOrder :: Suit -> Int
suitOrder Clubs    = 1
suitOrder Diamonds = 2
suitOrder Hearts   = 3
suitOrder Spades   = 4

rankOrder :: Rank -> Int
rankOrder (Left n) = n
rankOrder (Right (Ace _)) = 1
rankOrder (Right (Jack _)) = 11
rankOrder (Right (Queen _)) = 12
rankOrder (Right (King _)) = 13

-- 4.5

play :: [Card] -> Card -> Maybe Card
play [] _ = Nothing
play _ Joker = Nothing
play (Joker:xs) card = play xs card
play (Card your_suit your_rank:xs) (Card suit rank)
    | Card your_suit your_rank > Card suit rank =
        Just (Card your_suit your_rank)
    | otherwise =
        play xs (Card suit rank)

-- >>> play [Card Hearts (Left 2), Card Spades (Right (King Spades)), Joker] (Card Spades (Left 3))
-- Just (Card Spades (Right (King Spades)))

-- >>> play [Card Clubs (Left 5)] Joker
-- Nothing