module LexerEA(Token(..), PalRsv(..),  lexer, lexDigit) where

import Data.Char

{- ***************************************************************
   Definición del tipo de dato Token para clasificar cada uno de los
   componentes léxicos, los cuales pueden ser:
   a) Parentesis que abre y que cierra
   b) Números
   c) Operadores (+,*) 
   d) Palabras reservedas: Suc y Pred }
   ****************************************************************-}

data Token = ParA | ParC | Lit Int | Oper Char | Rsv PalRsv | Unkwn Char | Var String
  deriving Show

data PalRsv = Suc | Pred | In | End | LLet
  deriving Show

{- **************************************************************
   La función (lexer xs) recibe una cadena que corresponde a una 
   expresión aritmética en sintaxis concreta, conforme a las reglas
   de la gramática: 
     EA ::= T + EA | T
      T ::= F * T  | F
      F ::= Num | (EA) | Suc EA | Pred EA 
   Y devuelve la secuencia de tokens correspondientes.
   Por ejemplo: 
   *LexerEA> lexer "3+8*7"
    [Lit 3,Oper '+',Lit 8,Oper '*',Lit 7]
   *LexerEA> lexer "(3+8)*suc 7"
    [ParA,Lit 3,Oper '+',Lit 8,ParC,Oper '*',Rsv Suc,Lit 7]
   *LexerEA> lexer "(3+8)*suc 7+4"
    [ParA,Lit 3,Oper '+',Lit 8,ParC,Oper '*',Rsv Suc,Lit 7,Oper '+',Lit 4]
   *LexerEA> lexer "( 3 + 8 ) * suc ( 7 + 4 )"
    [ParA,Lit 3,Oper '+',Lit 8,ParC,Oper '*',Rsv Suc,ParA,Lit 7,Oper '+',Lit 4,ParC]
   **************************************************************-}
lexer :: String -> [Token]
lexer [] = []
lexer (' ' : xs) = (lexer xs)
lexer ('(' : xs) = ParA:(lexer xs)
lexer (')' : xs) = ParC:(lexer xs)
lexer ('+' : xs) = (Oper '+'):(lexer xs)
lexer ('-' : xs) = (Oper '-'):(lexer xs)
lexer ('*' : xs) = (Oper '*'):(lexer xs)
lexer ('/' : xs) = (Oper '/'):(lexer xs)
lexer ('=' : xs) = (Oper '='):(lexer xs)
lexer ('L':'e':'t':' ':xs) = (Rsv LLet):(lexer xs) 
lexer ('i':'n':' ':xs) = (Rsv In):(lexer xs)
lexer ('p':'r':'e':'d':' ':xs) = (Rsv Pred):(lexer xs)
lexer ('s':'u':'c':' ':xs) = (Rsv Suc):(lexer xs)
lexer ('e':'n':'d':' ':xs) = (Rsv End):(lexer xs)
lexer (x:xs)
        |isDigit (x) = lexDigit x xs
        |isAlpha x = Var v: (lexer c)
         where (v,c) = leeVar(x:xs) 
  
lexDigit x xs = let (digitos, resto) = break notDigit (x:xs);
                                     notDigit d = not (isDigit d);
                                     valDigit x = (ord x) - (ord '0');
                                     valDigits n [] = n;
                                     valDigits n (c:cs) = valDigits (10 * n + (valDigit c)) cs
            in 
                 (Lit (valDigits 0 digitos)):(lexer resto)



leeVar::String->(String, String)
leeVar [] = ("","")
leeVar (x:xs) = (takeWhile (isLetter) (x:xs), dropWhile (isLetter) (x:xs))


