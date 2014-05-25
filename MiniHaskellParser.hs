-- parser

module MiniHaskellParser where

import MiniHaskellAST

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellStyle)
import Data.Char

-- główna funkcja uruchamiająca parser
parseProgram :: String -> Either ParseError Program
parseProgram input = parse (do whiteSpace
                               p <- program
                               eof
                               return p) "" input

-- źródło poniższych linii: http://gist.github.com/89786
-- vvvvvvvvvvvvvvvvvvvvvvv
lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellStyle {
                             P.reservedNames = ["value", "data", "in", "case", "of", "lambda", "\\"],
                             P.reservedOpNames = ["=","|","->", "\\"] })

whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme lexer
symbol     = P.symbol lexer
parens     = P.parens lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer

-- ^^^^^^^^^^^^^^^^^^^^^^^
-- źródło powyższych linii: http://gist.github.com/89786

-- zmienna; musi zaczynać się od małej litery
var :: Parser Var
var = (do v <- identifier
          (if isLower $ head v
           then
               return v
           else
               fail "Variable should start with lowercase letter"))
      <?> "variable"

-- stała; musi zaczynać się od wielkiej litery
name :: Parser Name
name = (do n <- identifier
           (if isUpper $ head n
            then
                return n
            else
                fail "Variable should start with lowercase letter"))
       <?> "name"


program :: Parser Program
program = do many $ dataDecl <|> valueDecl
          <?> "program"

-- przykład: data N a b c = A a | B (b -> b) | C
dataDecl :: Parser Declaration
dataDecl = do reserved "data"
              constType <- lexeme constTypeDecl
              reservedOp "="
              constructors <- (lexeme typeDecl) `sepBy1` (reservedOp "|")
              return $ DataDeclaration constType constructors
           <?> "type declaration"

-- przykład: Car a (b -> b) C (D d)
constTypeDecl :: Parser TypeDeclaration
constTypeDecl = do n <- name
                   params <- many $ lexeme typeDecl
                   return $ ConstType n params
                <?> "constant type declaration"

-- przykład: (a -> b -> List a -> List b)
functionTypeDecl :: Parser TypeDeclaration
functionTypeDecl = do args <- parens $ (lexeme typeDecl) `sepBy1` (reservedOp "->")
                      return $ FunctionType args
                   <?> "function type declaration"

-- przykład: sizeOfScreen
varTypeDecl :: Parser TypeDeclaration
varTypeDecl = do v <- var
                 return $ VarType v
              <?> "variable type declaration"

-- try są potrzebne, gdyż gramatyka nie jest jednoznaczna;
-- przykałdowo, deklaracja typu może zaczynać się od małej litery, wtedy jest zmienną,
-- lub od nawiasu, wtedy jest albo wyrażeniem stałym w nawiasie albo deklaracją typu funkcji
typeDecl :: Parser TypeDeclaration
typeDecl = (try varTypeDecl) <|>
           (try $ parens constTypeDecl) <|> -- typeDecl jest rekurencyjna, mogą występować ciągi: F (A a) (B b)
           (try constTypeDecl) <|>
           (try functionTypeDecl)
           <?> "type declaration"

-- przykład: value x = \ a b -> f a b
valueDecl :: Parser Declaration
valueDecl = do reserved "value"
               var <- lexeme var
               reservedOp "="
               val <- lexeme expression
               return $ ValueDeclaration var val
            <?> "value declaration"

        
-- przykład: f (g a b) c
functionCallExpression :: Parser Expression
functionCallExpression = do f <- var
                            args <- many $ lexeme parenExpression
                            return $ FunctionCallExpression f args
                         <?> "function call expression"

variableCallExpression :: Parser Expression
variableCallExpression = do f <- var
                            return $ FunctionCallExpression f []
                         <?> "varaible call expression"

-- przykład: List (f a b) cs
constExpression :: Parser Expression
constExpression = do n <- lexeme name
                     args <- many $ lexeme parenExpression
                     return $ ConstExpression n args
                  <?> "const expression"

singleConstExpression :: Parser Expression
singleConstExpression = do n <- lexeme name
                           return $ ConstExpression n []
                        <?> "single const expression"

-- przykład: \ x y -> f x y
lambdaExpression :: Parser Expression
lambdaExpression = do reserved "\\"
                      vs <- many1 $ lexeme var
                      reservedOp "->"
                      e <- lexeme expression
                      return $ LambdaExpression vs e
                   <?> "lambda expression"

-- przykład: let value x = f y in g x
letExpression :: Parser Expression
letExpression = do reserved "let"
                   valueDecls <- many $ lexeme valueDecl
                   reserved "in"
                   e <- lexeme expression
                   return $ LetExpression valueDecls e
                <?> "let expression"

-- przykład: case f a b of C d -> g d | E -> No
caseExpression :: Parser Expression
caseExpression = do reserved "case"
                    e <- lexeme expression
                    reserved "of"
                    cs <- (lexeme caseOption) `sepBy1` (reservedOp "|")
                    return $ CaseExpression e cs
                 <?> "case expression"

-- część case
-- przykład: Succ a -> a
caseOption :: Parser (Expression,Expression)
caseOption = do t <- lexeme expression
                reservedOp "->"
                e <- lexeme expression
                return (t,e)
             <?> "one case's option"

-- try są potrzebne, gdyż gramatyka nie jest jednoznaczna;
-- przykładowo wyrażenie może zaczynać się od duzej litery, czyli był wyrażeniem stałym
-- lub od \, wtedy jest lambda-wyrażeniem
expression :: Parser Expression
expression = do ((try lambdaExpression) <|>
                 (try letExpression) <|>
                 (try caseExpression) <|>
                 (try functionCallExpression) <|>
                 (try constExpression) <|>
                 (try $ parens expression))
             <?> "expression"

parenExpression = do ((try singleConstExpression) <|>
                      (try variableCallExpression) <|>
                      (try $ parens expression))
                  <?> "paren expression"

instance Show Expression where
    show (ConstExpression name []) = name
    show (ConstExpression name es) = name ++ show es

instance Read Expression where
    readsPrec _ s = case parse expression "" s of
               Right e -> [(e,"")]
               Left er -> error $ show er