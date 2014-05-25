module Main where

import MiniHaskellParser
import MiniHaskellAST

import Text.ParserCombinators.Parsec
import List
import IO
import System( getArgs )

main = do args <- getArgs
          inh <- openFile (head args) ReadMode
	  code <- hGetContents inh
          case parseProgram code of
            Right program -> run program
            Left e -> putStrLn $ "parse error: " ++ show e

run :: Program -> IO ()
run program = case lookup "main" env of
                Just (ConstExpression "Dialog" [e]) -> runDialog env e
                Nothing -> putStrLn "main function not found"
    where env = extractVars program

extractVars :: Program -> Environment
extractVars = filtermap (\decl -> case decl of
                                    ValueDeclaration v e -> Just (v,e)
                                    _ -> Nothing)

filtermap :: (a -> Maybe b) -> [a] -> [b]
filtermap _ [] = []
filtermap f (x:xs) = case f x of
                       Nothing -> filtermap f xs
                       Just v -> v:filtermap f xs

type Environment = [(Var,Expression)]

-- oblicza wyrażenia, sprowadzając je do postaci ConstExpression lub LambdaExpression, w wypadku niewystarczającej ilości argumentów
eval :: Environment -> Expression -> Expression
eval env (ConstExpression name args) = ConstExpression name (map (eval env) args)
eval env (LetExpression decls e) = eval (extractVars decls ++ env) e
eval env (LambdaExpression vs e) = LambdaExpression vs e -- funkcja nie jest wywoływana, czeka na argumenty

-- wywołanie funkcji; w szczególności zero-argumentowej, czyli nie wskazującej na żadne lambda expression
eval env (FunctionCallExpression var argValues) = applicate (lookup var env)
    where applicate (Just (LambdaExpression argNames e)) = eval (zip argNames (map (eval env) argValues) ++ env) e -- funkcja
          applicate (Just e) = eval env e -- zmienna
          applicate Nothing = error $ "undefined symbol: " ++ var

eval env0 (CaseExpression e0 cases) = aux cases
    where aux ((c,e):cs) = case patternMatch (c, eval env0 e0) of
                             Just env1 -> eval (env1 ++ env0) e
                             Nothing -> aux cs
          aux [] = error "Pattern matching in case expression fails: uncaught"

-- homomorfizm
substitution :: Var -> Expression -> Expression -> Expression
substitution v arg (CaseExpression e es) = CaseExpression (substitution v arg e) (map (\(a,b) -> (a,substitution v arg b)) es)
substitution v arg (LambdaExpression vs e) = LambdaExpression vs (substitution v arg e)
substitution v arg (LetExpression ds e) = LetExpression ds (substitution v arg e)
substitution v arg (ConstExpression n es) = ConstExpression n (map (substitution v arg) es)
substitution v arg (FunctionCallExpression v2 es) | v == v2 = arg
                                                  | otherwise = FunctionCallExpression v2 (map (substitution v arg) es)

-- szczególny, prostszy przypadek unifikacji
patternMatch :: (Expression,Expression) -> Maybe Environment
patternMatch (FunctionCallExpression v [],e) = Just [(v,e)]
patternMatch (ConstExpression n1 es1,ConstExpression n2 es2) | n1 == n2 = joinEnvs $ map patternMatch $ zip es1 es2
                                                             | otherwise = Nothing
    where joinEnv2 (Just a) (Just b) = Just (a++b)
          joinEnv2 _ _ = Nothing
          joinEnvs envs = foldr joinEnv2 (Just []) envs
patternMatch (_,_) = Nothing

{-
Dialogi

Źródło:
Imperative Functional Programming (1993)
by Simon L. Peyton Jones, Philip Wadler

Poniższy kod wyglądałby tak, gdybym implementował Dialogi za pomocą monad w czystym Haskellu:

process :: Request -> IO Response
process Read = do c <- getChar
                  return $ Value c
process (Write value) = do putChar value
                           return OK

bottom = error "bottom"
tailC (Cons _ r) = r

runDialog :: (List Response -> List Request) -> IO ()
runDialog dialog =
    case dialog bottom of
      Nil -> return ()
      Cons request rest -> do r <- process request
                             runDialog $ \rest -> tailC (dialog $ Cons r rest)
-}

-- niestety, w trakcie translacji dialogów do minihaskella utracono informacje o typach;
-- oznacza to, że nie jestem w stanie odróżnić Request od Response, oba mają typ Expression;
-- skutkuje to nieintuicyjną sygnaturą typu funkcji process
process :: Expression -> IO Expression
process (ConstExpression "Read" []) = do line <- getLine
                                         return $ ConstExpression "ReadOK" [read line]
process (ConstExpression "Write" [e]) = do putStrLn $ show e
                                           return $ ConstExpression "OK" []
bottom :: Expression
bottom = error "bottom"

tailC (ConstExpression "Cons" [_,rest]) = rest
tailC _ = ConstExpression "Nil" []
headC (ConstExpression "Cons" [h,_]) = h

-- Jak widać, nie jest to bezproblemowa translacja z Haskella na minihaskella
-- Przede wszystkim należało rozwiązać problem przekazywania \rest -> ...
-- Rozwiązałem to w ten sposób, że manipulowałem kodem źródłowym, aby osiągnąć odroczone obliczenie.
-- Kluczowym fragmentem jest zamiana początkowego fragmentu strumenia na lambda-wyrażenie oraz
-- specjalne traktowanie tej lambdy przy kolejnych wywołaniach rekurencyjnych.
-- Aby każde z kolejnych lambda-wyrażeń miało swoją własną zmienną, zastosowałem funkcję succ, czyli następną wolną zmienną
-- (zmienna jest wolna, gdyż oryginalne zmienne nie mogą mieć ', a succ dodaje ' na koniec)
runDialog :: Environment -> Expression -> IO ()
runDialog env (LambdaExpression [var] body) =
    case eval ((var,bottom):env) body of
      (ConstExpression "Nil" []) -> return ()
      (ConstExpression "Cons" [request,rest]) ->
          do resp <- process request
             runDialog ((succ var,rest):env) $ LambdaExpression [succ var]
                           $ tailC (substitution var (ConstExpression "Cons" [resp,FunctionCallExpression (succ var) []]) body)
    where succ :: Var -> Var
          succ = (++)"\'"