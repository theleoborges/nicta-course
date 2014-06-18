add :: Int -> (Int -> Int)
add x y = x + y

add' :: Int -> Int -> Int
add' x = \y -> x + y


add'' :: Int -> Int -> Int
add'' = \x -> \y -> x + y

add''' :: Int -> Int -> Int
add''' = \x y -> x + y


hof :: (Int -> Int) -> Int
hof f = f 3

data Shape =
     Circle Int
     | Square Int
     | Rectangle Int Int
     deriving (Eq, Show)

perim :: Shape -> Int
perim (Circle r) = r * 2 * 3
perim (Square s) = s * 4
perim (Rectangle w h) = w * 2 + h * 2

perim' :: Shape -> Int
perim' s = case s of
       Circle r -> undefined
       Square s -> undefined
       Rectangle w h -> undefined



first :: a -> a -> a
first x _ = x

-- first (+) (-) 1 2

data Container a  = C a

blab :: (a -> b) -> Container a -> b
blab f (C a) = f a

-- blab (\x -> x * x) (C 10)

blab' :: (a -> b) -> Container a -> Container b
blab' f (C a) = C(f a)

data Fred a = F a | a :? a
     deriving (Eq, Show)
