
module ParserEA where

import LexerEA

{- ***************************************************************
   Definición del tipo de dato AsaEA, que corresponde a un árbol
   de sintáxis abstracta de una expresión aritmética, 
   por la siguiente gramática. 
     AsaEA ::= Cons Int             (Para representar los números)
              | Sum AsaEA AsaEA     (Para representar las sumas)  
              | Prod AsaEA AsaEA    (Para representar las multiplicaciones)  
              | Scs AsaEA           (Para representar la función sucesor)  
              | Prd AsaEA           (Para representar la función predecesor)  
   ****************************************************************-}

data AsaEA = Const Int 
              | Sum AsaEA AsaEA
              | Res AsaEA AsaEA
              | Prod AsaEA AsaEA
              | Div AsaEA AsaEA
              | Scs AsaEA 
              | Prd AsaEA
             deriving Show

{- ***************************************************************
   La función (parser t) recibe una lista de tokens y construye el 
   árbol de sintáxis abstracta correspondiente a la expresión,
   siempre y cuando está corresponda a las reglas de la gramática
     EA ::= T + EA | T
      T ::= F * T  | F
      F ::= Num | (EA) | Suc EA | Pred EA 
  Para esto, utiliza funciones axiliares que se encargan de 
  analizar cada categoria.

-- Por ejemplo: 
--
-- ParserEA> parser [Lit 3,Oper '+',Lit 8,Oper '*',Lit 7]
--  Sum (Const 3) (Prod (Const 8) (Const 7))
--
-- ParserEA> parser (lexer "3+8*7+suc (2*1)")
--  Sum (Const 3) 
--      (Sum (Prod (Const 8) (Const 7)) 
--           (Scs (Prod (Const 2) (Const 1))))
--
-- ParserEA> parser (lexer "3+8*7+suc 2*1")
--  Sum (Const 3) 
--      (Sum (Prod (Const 8) (Const 7)) 
--           (Prod (Scs (Const 2)) (Const 1)))
--
  ****************************************************************-}
parser :: [Token] -> AsaEA
parser t = 
  case (parserE t) of
    (expr, []) -> expr
    _          -> error "Análisis sintáctico fallido."



-- *************************************************************
-- (parserE t), recibe una lista de tokens t, y devuelve una
-- una un AsaEA con la primera expresión que puede formar a 
-- partir de t y el resto de tokens que faltan por analizar  
--
-- Por ejemplo: 
--
-- ParserEA> parserE (lexer "4*9)+2")
--(Prod (Const 4) (Const 9),[ParC,Oper '+',Lit 2])
-- ParserEA> parserE (lexer "suc 4*9)+2")
--(Prod (Scs (Const 4)) (Const 9),[ParC,Oper '+',Lit 2])
-- ParserEA> parserE (lexer "suc 4+9)+2")
--(Sum (Scs (Const 4)) (Const 9),[ParC,Oper '+',Lit 2])
--
-- ************************************************************
parserE::[Token] -> (AsaEA, [Token])      
parserE tokens =                        
  case rst of                          
     []                  -> (e1', [])    
     (Oper '+'):rst'    -> let         
                            (e2', rst'') =  parserE rst'  
                          in (Sum e1' e2', rst'')
     (Oper '-'):rst'    -> let         
                            (e2', rst'') =  parserE rst'  
                          in (Res e1' e2', rst'')  
     _  -> (e1', rst)
   where 
    (e1', rst) = parserT tokens


-- *************************************************************
-- (parserT t), analiza expresiones de la categoria T (términos) 
-- recibe una lista de tokens t, y devuelve un
--  AsaEA con la primera expresión con un producto 
--  y el resto de tokens que faltan por analizar  
--
-- Por ejemplo: 
--
--ParserEA> parserT (lexer "(4+9)+2")
--(Sum (Const 4) (Const 9),[Oper '+',Lit 2])
--ParserEA> parserT (lexer "3*2)+3")
--(Prod (Const 3) (Const 2),[ParC,Oper '+',Lit 3])
--ParserEA> parserT (lexer "4+9)+2")
--(Const 4,[Oper '+',Lit 9,ParC,Oper '+',Lit 2])
--ParserEA> parserT (lexer "suc 4+9)+2")
--(Scs (Const 4),[Oper '+',Lit 9,ParC,Oper '+',Lit 2])
--ParserEA> parserT (lexer "suc (4+9)+2")
--(Scs (Sum (Const 4) (Const 9)),[Oper '+',Lit 2])
--
-- ************************************************************

parserT:: [Token] -> (AsaEA, [Token])    
parserT tokens =                       
  case rst of 
    []                  -> (e1', [])
    (Oper '*'):rst'     -> let
                             (e2', rst'') =  parserF rst'
                           in (Prod e1' e2', rst'')
    (Oper '/'):rst'     -> let
                             (e2', rst'') =  parserF rst'
                           in (Div e1' e2', rst'')
    _                   -> (e1',rst)

  where 
    (e1', rst) = parserF tokens


-- *************************************************************
-- (parserT t), analiza expresiones de la categoria F (factores) 
-- recibe una lista de tokens t, y devuelve un
--  AsaEA con la primera expresión de la categoria F 
--  y el resto de tokens que faltan por analizar  
--
-- Por ejemplo: 
--
--ParserEA> parserF (lexer "2+3")
--(Const 2,[Oper '+',Lit 3])
--ParserEA> parserF (lexer "3*2+3")
--(Const 3,[Oper '*',Lit 2,Oper '+',Lit 3])
--ParserEA> parserF (lexer "(3*2)+3")
--(Prod (Const 3) (Const 2),[Oper '+',Lit 3])
--ParserEA> parserF (lexer "suc 3*2)+3")
--(Scs (Const 3),[Oper '*',Lit 2,ParC,Oper '+',Lit 3])
--
-- ************************************************************
parserF:: [Token] -> (AsaEA, [Token])   

parserF ((Lit n):tkns) = (Const n, tkns) -- Literales numéricas

parserF (ParA:tkns)     =                   --- Expresiones parentizadas (E)
  let
    (e', restTkns) = parserE tkns
  in case  restTkns of
    (ParC: restTkns') -> (e',  restTkns')
    _                -> error ("falta un paréntesis que cierra " ++ show restTkns)


parserF (Rsv Suc:tokens) = (Scs tkns2, rest)   -- Sucesor
 where
--  (Op Osuc) : tkns1 = tokens
  (tkns2,rest) = parserF tokens
  


parserF (Rsv Pred:tokens) = (Prd tkns2, rest)  -- Predecesor
 where
  (tkns2,rest) = parserF tokens


parserF tokens = error ("Error gramatical iniciando en : " ++ show tokens)

-- eval :: AsaEA -> Int

-- eval (Const n) = n
-- eval (Sum x y) = (eval x) + (eval y)
-- eval (Res x y) = (eval x) - (eval y)
-- eval (Prod x y) = (eval x) * (eval y)
-- eval (Div x y) = div (eval x) (eval y)


type PControl = [Op]
data Op = METE AsaEA
        | SUM Int
        | RES Int
        | PROD Int 
        | DIV Int
        | SUC AsaEA
        | PRED AsaEA

eval :: AsaEA -> PControl -> Int
eval ( Const n ) p = ejec p n
eval ( Sum x y ) p = eval x ( METE y : p )
eval ( Res x y ) p = eval x ( METE y : p )
eval ( Prod x y ) p = eval x ( METE y : p )
eval ( Div x y ) p = eval x ( METE y : p )

ejec :: [ Op ] -> Int -> Int
ejec [] n = n
ejec ( METE y : p ) n = eval y ( SUM n : p )
ejec ( SUM n : p ) m = ejec p ( n + m )
ejec ( RES n : p ) m = ejec p ( n - m )
ejec ( PROD n : p ) m = ejec p ( n * m )
ejec ( DIV n : p ) m = ejec p (div n  m )

interp :: AsaEA -> Int
interp e = eval e []