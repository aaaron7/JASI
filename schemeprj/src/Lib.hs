{-# LANGUAGE ExistentialQuantification #-}


module Lib where

import Text.ParserCombinators.Parsec hiding(spaces)
import Control.Monad
import Control.Monad.Error
import Data.IORef
import Envir
import Global
import System.IO
import Numeric
import Data.Char(digitToInt)
-- import qualified Data.Attoparsec.ByteString as ATT

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)


spaces :: Parser ()
spaces = skipMany1 space


parseString :: Parser LispVal
parseString = do
  char '"'
  x <- parseInnerString
  return $ String x

parseInnerString :: Parser String
parseInnerString = do
  c <- anyChar
  case c of
    '"' -> return []
    '\\' -> do
      sc <- anyChar
      cs <- parseInnerString
      return $ c:sc:cs
    otherwise -> do
      cs <- parseInnerString
      return $ c:cs




parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber ::Parser LispVal
parseNumber = parseDec <|>  do
  char '#'
  parseHex <|> parseOct <|> parseBinary

parseDec :: Parser LispVal
parseDec = do
  str <- many1 digit
  return $ Number (read str)

parseOct :: Parser LispVal
parseOct = do
  char 'o'
  str <- many1 (satisfy isOct)
  return $ Number $ fst $ head (readOct str)
  where
    isOct c = c `elem` "01234567"

parseBinary :: Parser LispVal
parseBinary = do
  char 'b'
  str <- many1 (satisfy (\x -> x `elem` "01"))
  return $ Number $ fst $ head (readInt 2 (\_->True) digitToInt str)

parseHex :: Parser LispVal
parseHex = do
  char 'x'
  str <- many1 (satisfy isHex)
  return $ Number $ fst $ head (readHex str)
  where
    isHex c = c `elem` "0123456789ABCDEFabcdef"

parseChar :: Parser LispVal
parseChar = do
    try (string "#\\space")
    return $ Character "#\\space"
  <|> do
    try (string "#\\newline")
    return $ Character "#\\newline"
  <|> do
    string "#\\"
    c <- anyChar
    case c of
      ' ' -> return $ Character "#\\space"
      otherwise -> return $ Character ("#\\" ++ [c])

parseFloat :: Parser LispVal
parseFloat = do
  num1 <- many1 digit
  char '.'
  num2 <- many1 digit
  return $ Float $ fst $ head $ (readFloat (num1 ++ "."++num2))



parseExpr :: Parser LispVal
parseExpr = try parseFloat <|> try parseNumber  <|> try parseChar <|> parseAtom <|> parseString <|>  parseQuoted <|> do
  char '('
  x <- try parseList <|> parseDottedList
  char ')'
  return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote",x]


makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal



eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return  val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Character _) = return val
eval env val@(Float _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote" , val]) = return val
eval env (List [Atom "if", pred,conseq,alt]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    Bool True -> eval env conseq
    otherwise ->  throwError $ TypeMismatch "bool" result

eval env (List (Atom "cond" : stmts)) = do
  results <-  filterM checkPred stmts
  if length results <= 0 then throwError $ Default "didn't match any pattern" else execResult $ head results
  where
    checkPred (List [Atom "else",_]) = return True
    checkPred (List [pred,_]) = eval env pred >>= liftThrows . unpackBool
    execResult (List [_,value]) = eval env value

eval env (List (Atom "case" : pred : conditions)) = do
  val <- eval env pred
  results <- liftThrows $ filterM (checkPred val) conditions
  if length results <= 0 then throwError $ Default "didn't match any pattern" else execResult $ head results
  where
    checkPred val (List [Atom "else",_]) = return True
    checkPred val (List [scales,_]) = member val scales
    member val (List eles) = fmap ((not . null) . filter id) (mapM (checkEqual val) eles )
    checkEqual x y = eqv [x,y] >>= unpackBool
    execResult (List [_,value]) = eval env value

eval env (List [Atom "set!", Atom var,form]) =
  eval env form >>= setVar env var

eval env (List [Atom "define", Atom var,form]) =
  eval env form >>= defineVar env var


eval env (List (Atom "define" : List (Atom var : params) : body )) =
  makeNormalFunc env params body >>= defineVar env var

eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var

eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body

eval env (List (Atom "lambda" : DottedList params varargs : body)) = makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval env)




eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals


eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


-- apply :: String -> [LispVal] -> ThrowsError LispVal
-- apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
--                         ($ args)
--                         (lookup func primitives)

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args = if num params /= num args && varargs == Nothing
  then throwError $ NumArgs (num params) args
  else (liftIO $ bindVars closure $ zip params args) >>= bingVarArgs varargs >>= evalBody
  where num = toInteger . length
        remainingArgs = drop (length params) args
        evalBody env = liftM last $ mapM (eval env) body
        bingVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName , List $ remainingArgs)]
          Nothing -> return env
apply (IOFunc func) args = func args

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives ++  map (makeFunc PrimitiveFunc) primitives) where
  makeFunc constructor (var,func) = (var,constructor func)


applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args


makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc  :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port ) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj,Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename


primitives :: [(String,[LispVal] ->ThrowsError LispVal)]
primitives = [
  ("+",numericBinop(+)),
  ("-",numericBinop(-)),
  ("*",numericBinop(*)),
  ("+",numericBinop div),
  ("mod",numericBinop mod),
  ("quotient",numericBinop quot),
  ("remainder",numericBinop rem),
  ("=",numBoolBinop (==)),
  ("<",numBoolBinop (<)),
  (">",numBoolBinop (>)),
  ("/=",numBoolBinop (/=)),
  (">=",numBoolBinop (>=)),
  ("<=",numBoolBinop (<=)),
  ("&&",boolBoolBinop (&&)),
  ("||",boolBoolBinop (||)),
  ("string=?",strBoolBinop (==)),
  ("string<?",strBoolBinop (<)),
  ("string>?",strBoolBinop (>)),
  ("string<=?",strBoolBinop (<=)),
  ("string>=?",strBoolBinop(>=)),
  ("car",car),
  ("cdr",cdr),
  ("cons",cons),
  ("eq?",eqv),
  ("eqv?",eqv),
  ("equal?",equal),
  ("symbol?",isSymbol),
  ("string?",isString),
  ("number?",isNumber)
  ]


ioPrimitives :: [(String,[LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply",applyProc),
                ("open-input-file",makePort ReadMode),
                ("open-output-file",makePort WriteMode),
                ("close-input-file",closePort),
                ("close-output-file",closePort),
                ("read",readProc),
                ("write",writeProc),
                ("read-contents",readContents),
                ("read-all",readAll)]




boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                              then throwError $ NumArgs 2 args
                              else do
                                left<- unpacker $ args !! 0
                                right <- unpacker $ args !! 1
                                return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool



isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number _] = return $ (Bool True)
isNumber _ = return $ (Bool False)


isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ (Bool True)
isString _ = return $ (Bool False)



isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ ( Bool True)
isSymbol _ = return $ ( Bool False)




numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] ->ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op


unpackNum :: LispVal ->ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer ,String)] in
                          if null parsed then throwError $ TypeMismatch "number" $ String n else return $ fst $ parsed !! 0

unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum





car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _ ] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList


cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1 ,List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x:xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1,x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList


eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1) ,(String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1) , (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x] , List  $ ys ++[y]]
eqv [(List arg1) , (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
  where eqvPair (x1,x2)= case eqv [x1,x2] of
                          Left err -> False
                          Right (Bool val) -> val
eqv [_,_] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList


unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [List (x:xs) , List (y:ys)] = do
  headEquals <- equal [x,y]
  tailsEquals <- equal [(List xs),(List ys)]
  return $ Bool $ (let (Bool h) = headEquals in h && let (Bool t) = tailsEquals in t)


equal [arg1 , arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                      [AnyUnpacker unpackNum, AnyUnpacker unpackStr , AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1,arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList



someFunc :: IO ()
someFunc = putStrLn "someFunc"
