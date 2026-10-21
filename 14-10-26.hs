main :: IO()

isJust :: Maybe a -> Bool

isJust (Just x) = True
isJust _ = False

fromMaybe :: a -> Maybe a -> a 

fromMaybe _ (Just a) = a 
fromMaybe a _ = a

forget :: Either String a -> Maybe a 

forget (Left _) = Nothing
forget (Right a) = Just a

-- Just (Right True) is of type Maybe (Either a Bool)

safeDiv :: Int -> Int -> Maybe Int 

safeDiv _ 0 = Nothing
safeDiv n a = Just (n `div` a)

safeDiv' :: Int -> Int -> Either String Int 

safeDiv' _ 0 = Left "Division by zero"
safeDiv' n a = Right (n `div` a)

data Expr = Lit Int
    | Add Expr Expr 
    | Mul Expr Expr 
    | Sub Expr Expr

size :: Expr -> Int

size (Lit n) = 0
size (Add expr expr') = 1 + size (expr) + size (expr')
size (Mul expr expr') = 1 + size (expr) + size (expr')
size (Sub expr expr') = 1 + size (expr) + size (expr')

eval :: Expr -> Int 

eval (Lit n) = n
eval (Add expr expr') = eval expr + eval expr'
eval (Mul expr expr') = eval expr * eval expr'
eval (Sub expr expr') = eval expr - eval expr'

pretty :: Expr -> String 

pretty (Lit n) = show n
pretty (Add expr expr') = pretty expr ++ "+" ++ pretty expr'
pretty (Mul expr expr') = pretty expr ++ "*" ++ pretty expr'
pretty (Sub expr expr') = pretty expr ++ "-" ++ pretty expr'

addTwo :: Num a => Either String a -> Either String a

addTwo (Right a) = Right (a + 2) 
addTwo _ = Left "Not a number!"


main = do print (addTwo (Left "bruh"))