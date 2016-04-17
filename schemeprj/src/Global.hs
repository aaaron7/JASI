{-# LANGUAGE ExistentialQuantification #-}

module Global where

import Control.Monad.Error
import Data.IORef
import Text.ParserCombinators.Parsec hiding(spaces)
import GHC.IO.Handle

type Env = IORef [(String,IORef LispVal)]


type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | Character String
            | String String
            | Float Float
            | Bool Bool
            | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
            | Func {params :: [String] , vararg :: (Maybe String),body :: [LispVal],closure :: Env}
            | IOFunc ([LispVal] -> IOThrowsError LispVal)
            | Port Handle



data LispError = NumArgs Integer [LispVal]
              | TypeMismatch String LispVal
              | Parser ParseError
              | BadSpecialForm String LispVal
              | NotFunction String String
              | UnboundVar String String
              | Default String

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default




extractValue :: ThrowsError a -> a
extractValue (Right val) = val

instance Show LispVal where show = showVal
instance Show LispError where show = showError



showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parser error at " ++ show parseErr
showError (Default str) = "error : " ++ str

unwordsList :: [LispVal] -> String
unwordsList = unwords.map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom names) = names
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs}) = "(lambda (" ++ unwords (map show args) ++
                        (case varargs of
                          Nothing -> ""
                          Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO Port>"
showVal (IOFunc _) = "<IO primitive>"
showVal (Character s) = s
showVal (Float s) = show s



trapError action = catchError action (return . show)
