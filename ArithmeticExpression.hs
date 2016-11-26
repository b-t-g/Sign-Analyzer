module ArithmeticExpression where

data ArithmeticExpression = ArithmeticExpression { firstExp :: ArithmeticExpression
                                                 , op :: Operator
                                                 , secondExp :: ArithmeticExpression
                                                 } | Singleton { sign :: Sign} deriving Eq

data Operator = Oplus | Otimes deriving Eq

data Sign = Plus | Minus | Zero deriving (Eq, Ord)

instance Show ArithmeticExpression where
  show (ArithmeticExpression a1 o a2) = show a1 ++ " " ++ (show o) ++ " " ++ show a2
  show (Singleton i) = show i

instance Show Operator where
  show (Oplus) = "\8853"
  show (Otimes) = "\8855"

instance Show Sign where
  show Plus = "{+}"
  show Minus = "{-}"
  show Zero = "{0}"
