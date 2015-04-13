{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Prelude hiding (takeWhile)
import           Datatypes
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString (ByteString)

ident :: Parser ByteString
ident = BS.cons <$> satisfy (inClass alphabet) <*> takeWhile (inClass x)
    where f _ _ = Nothing
          alphabet = "A-Za-z"
          numbers  = "0-9_"
          x = alphabet ++ numbers

          
junk :: Parser () 
junk = () <$ takeWhile isSpace
          
parens :: Parser x -> Parser x
parens x = char '(' *> junk *> x <* junk <* char ')'
          
    
word :: ByteString -> Parser ByteString
word x = junk *> string x <* junk
          
variable :: Parser Variable
variable = Variable . BS.unpack <$> (junk *> ident)

block :: Parser Block
block = junk *> char '{' *> junk *>
        (Block <$> many statement)
        <* junk <* char '}'

mchoice :: (a -> Parser b) -> [a] -> Parser b
mchoice f = choice . map f
        
typePlaceholder :: Parser () 
typePlaceholder = () <$ (optional
    (mchoice f
        ["void"
        ,"int"
        ,"bool"]))
    where f s = string s
      
definition :: Parser Definition 
definition = junk *> typePlaceholder <* junk *> do
        x <- variable
        junk
        def x 
    where def x = deffun x <|> defvar x
      
defvar :: Variable -> Parser Definition
defvar x =
    DefVar x <$> (optional (assigOp *> junk *> expr))

deffun :: Variable -> Parser Definition
deffun x = DefFun x <$> parens (sepBy variable (char ','))
                    <*> block
    
    
        
statement :: Parser Statement
statement = junk *> (sif <|> swhile <|> sassig <|> sdef <|> sfcall)
            <* junk <* char ';'

sif :: Parser Statement
sif = 
    word "if" *> 
     (Sif <$> parens expr
          <*> block
          <*> (maybe (Block []) id <$> optional block))
         
         
swhile :: Parser Statement
swhile =
    word "while" *>
     (Swhile <$> parens expr
             <*> block)


sdef :: Parser Statement
sdef = Sdef <$> definition

assigOp :: Parser ()
assigOp = () <$
    try (choice (map string
         [":="
         ,"="
         ,"<-"]))
                    
    
sassig :: Parser Statement
sassig =
    Sassig <$> variable
           <*> (assigOp *> expr)
           
sepByIgn :: Parser x -> Parser sep -> Parser [Maybe x]
sepByIgn elm sep = go
    where go = do 
            p <- junk *> optional elm
            s <- junk *> optional sep
            case s of
                 Nothing -> return [p]
                 Just _  -> (p:) <$> go
           
sfcall :: Parser Statement
sfcall =
    Sfcall <$> variable 
           <*> parens (sepByIgn expr (char ','))
           
efcall :: Parser Expr
efcall =
    Efcall <$> expr
           <*> parens (sepBy expr (char ','))

operator :: ByteString -> (Expr -> Expr -> x) -> Parser x
operator op f = do
    x <- expr
    word op
    y <- expr
    return (f x y)

numop :: ByteString -> (Int -> Int -> Int) -> Parser Expr
numop op f = operator op (EIntOP (F f))
 -- need some less dirty way to deal with a*b+c such that it doesn't get parsed incorrectly
    
eequ :: Parser Expr
eequ = operator "==" EEqu

eand :: Parser Expr
eand = operator "&&" EAnd

eor  :: Parser Expr
eor  = operator "||" EOr

eplus :: Parser Expr
eplus = numop "+" (+)

eminus :: Parser Expr
eminus = numop "-" (-)

       
expr :: Parser Expr
expr = junk *>
   (parens expr 
   <|> varlookup
   <|> eequ
   <|> eand
   <|> eor
   <|> efcall)

varlookup :: Parser Expr
varlookup = ELookup <$> variable

program :: Parser Program
program =
    (Program <$> many (junk *> definition)) <* junk