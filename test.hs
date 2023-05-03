data Calc where
    Lit :: Double -> Calc
    Una :: Op -> Calc -> Calc
    Bin :: Op -> Calc -> Calc -> Calc
    deriving Show

data Op where
    Plus :: Op
    Minus :: Op
    Times :: Op
    Div :: Op
    Power :: Op
    Neg :: Op
    deriving (Show, Eq)

type Precedence = Int

data Associativity where
    L :: Associativity
    R :: Associativity

prec :: Op -> Precedence
prec Neg = 5
prec Times = 4
prec Div = 4
prec