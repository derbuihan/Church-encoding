class ToInt a where
  toInt :: a -> Integer

data N = Z | S N deriving (Eq)

instance ToInt N where
  toInt Z = 0
  toInt (S n) = 1 + toInt n

instance Show N where
  show n = show (toInt n)

instance Ord N where
  compare Z Z = EQ
  compare Z (S _) = LT
  compare (S _) Z = GT
  compare (S n) (S m) = compare n m

instance Num N where
  Z + n = n
  (S n) + m = n + (S m)

  Z - _ = Z
  n - Z = n
  (S n) - (S m) = n - m

  Z * _ = Z
  _ * Z = Z
  (S n) * m = m + (n * m)
  
  negate = id

  abs = id

  signum n = S Z

  fromInteger n
    | n == 0 = Z
    | n > 0 = S (fromInteger (n - 1)) 
    | otherwise = Z

data I = I N N deriving Show

reduction (I Z b) = I Z b
reduction (I a Z) = I a Z
reduction (I (S a) (S b)) = reduction (I a b)

instance ToInt I where
  toInt (I a b)
    | a >= b = toInt (a - b)
    | otherwise = negate $ toInt (b - a)

instance Eq I where
  (I a a') == (I b b') = (a + b') == (b + a')

instance Ord I where
  compare (I a a') (I b b')
    | (a + b') > (b + a') = GT
    | (a + b') == (b + a') = EQ
    | (a + b') < (b + a') = LT

instance Num I where
  (I a a') + (I b b') = reduction (I (a + b) (a' + b'))

  (I a a') - (I b b') = reduction (I (a + b') (a' + b))

  (I a a') * (I b b') = reduction (I (a * b + a' * b') (a * b' + a' * b))

  negate (I a b) = (I b a)

  abs (I a b) = reduction ((I a b) * signum (I a b))

  signum (I a b)
    | a >= b = I (S Z) Z
    | otherwise = I Z (S Z)

  fromInteger n
    | n >= 0 = I (fromInteger n) 0
    | otherwise = I 0 (fromInteger (negate n))

