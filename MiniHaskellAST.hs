module MiniHaskellAST where

-- struktury danych, w których trzymane są abstrakcyjne drzewa rozbioru

type Var = String -- rozpoczynający się od małej litery
type Name = String -- rozpoczynający się od dużej litery

type Program = [Declaration]

data Declaration = ValueDeclaration Var Expression | -- nazwa zmiennej, wyrażenie
                   DataDeclaration TypeDeclaration [TypeDeclaration] -- nazwa, konstruktory

data TypeDeclaration = VarType Var | -- nazwa zmiennej
                       ConstType Name [TypeDeclaration] | -- Nazwa typu, parametry typu
                       FunctionType [TypeDeclaration] -- typ1 -> typ2 -> ... -> typn

data Expression = CaseExpression Expression [(Expression,Expression)] | -- case (1) of (2) -> (3)
                  LambdaExpression [Var] Expression | -- \ (1) -> (2)
                  LetExpression [Declaration] Expression | -- let (1) in (2)
                  ConstExpression Name [Expression] |
                  FunctionCallExpression Var [Expression] -- dodatkowo wyłuskanie wartości zmiennej, gdy (2) = []

