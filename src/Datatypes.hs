module Datatypes where

import Data.Map (Map)

newtype Variable = Variable String
  deriving (Show,Eq)

data Program    = Program [Definition]
  deriving (Show,Eq)

newtype Block = Block [Statement]
  deriving (Show,Eq)
  
data Definition = DefFun Variable [Variable] Block
                | DefVar Variable (Maybe Expr)
  deriving (Show,Eq)
                
data Statement = Sif    Expr Block Block
               | Swhile Expr Block
               | Sassig Variable Expr
               | Sdef   Definition
               | Sfcall Variable [Maybe Expr] -- the only functions meant to have the posibility of ignoring parameters are the ones managing memory (?)
               | Sexpr  Expr
  deriving (Show,Eq)
  
newtype F x = F x

instance Show (F x) where
    show _ = "Function"
instance Eq   (F x) where
    (==) _ _ = False=
    
data Expr = EIntOP  (F (Int -> Int -> Int)) Expr Expr
          | EEqu    Expr Expr
          | EAnd    Expr Expr
          | EOr     Expr Expr
          | Value   MCell
          | ENot    Expr
          | ELookup Variable
          | Efcall  Expr [Expr] 
    deriving (Show,Eq)
    
newtype MDirection = MDirection Int
  deriving (Show,Eq)
  
data MCell = MNumber  Int
           | MBoolean Bool
           | MStruct  [MCell]
           | MPointer MDirection
           | MNULL
  deriving (Show,Eq)
   
newtype Memory = Memory (Map MDirection MCell) 
  deriving (Show,Eq)
  
