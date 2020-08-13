data D a = D a a deriving (Eq, Show)

-- | AD has something to do with calculating a function's
--   values and derivative values simultaneously.

constD :: Num a => a -> D a
constD x = D x 0

idD :: Num a => a -> D a
idD x = D x 1

instance Num a => Num (D a) where
  (+) (D x x') (D y y') = D (x + y) (x' + y')
  (*) (D x x') (D y y') = D (x * y) (x' * y')
  negate (D x x') = D (negate x) (negate x')
  abs (D x x') = D (abs x) (x' * signum x)
  signum (D x _) = D (signum x) 0
  fromInteger x = constD (fromInteger x)

instance Fractional a => Fractional (D a) where
  fromRational x = constD (fromRational x)
  recip (D x x') = D (recip x) (-1 * x' / sqr x)

sqr :: Num a => a -> a
sqr x = x * x

instance Floating a => Floating (D a) where
  pi = constD pi
  exp (D x x') = D (exp x) (x' * exp x)
  log (D x x') = D (log x) (x' / x)
  sin (D x x') = D (sin x) (x' * (cos x))
  cos (D x x') = D (sin x) (x' * (-1 * sin x))
  asin (D x x') = D (asin x) (x' / sqrt (1 - sqr x))
  acos (D x x') = D (acos x) (x' / (-1 * sqrt (1 - sqr x)))
  sinh (D x x') = D (sinh x) (x' * cosh x)
  cosh (D x x') = D (cosh x) (x' * sinh x)
  asinh (D x x') = D (asinh x) (x' / sqrt (1 + sqr x))
  acosh (D x x') = D (acosh x) (x' / sqrt (sqr x - 1))
  atan (D x x') = D (atan x) (x' / (1 + sqr x))
  atanh (D x x') = D (atanh x) (x' / (1 - sqr x))
