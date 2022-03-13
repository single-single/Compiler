{-
Compilers (COMP3012) Coursework, 2021
  Shiqi XIN - 20125731
  (based on: Venanzio Capretta
             Nicolai Kraus)

Parser for MiniTriangle, generating an Abstract
Syntax Tree from a program (a string).
Uses functional parsers.
-}

module MiniTriangleParser where

-------------- Library Files ---------------

import Lib.FunParser
import Control.Applicative
import Data.List (elem)

------------- Basic Definitions ------------

type Identifier = String

keywords :: [String]
keywords =
       ["let", "in", "var", "if", "then",
       "else", "while", "do", "getint",
       "printint", "begin", "end"]

data BinOperator = Addition | Subtraction | Multiplication | Division
                 | Conjunction | Disjunction
                 | LssOp | LeqOp | GtrOp | GeqOp | EqOp | NeqOp
  deriving (Eq,Show,Enum)

data UnOperator = Negation | NegBool
  deriving (Eq,Show)

data Program = Prog Declarations Command
  deriving (Eq, Show)

data Declaration = Decl Identifier
                 | DeclAndInit Identifier Expr
  deriving (Eq, Show)

type Declarations = [Declaration]

data Command = CmdAssign Identifier Expr
             | CmdIf Expr Command Command
             | CmdWhile Expr Command
             | CmdGet Identifier
             | CmdPrint Expr
             | CmdBegin Commands
  deriving (Eq, Show)

type Commands = [Command]

data Expr = LitInteger Int
         | Variable Identifier
         | BinOp BinOperator Expr Expr
         | UnOp  UnOperator Expr
         | Conditional Expr Expr Expr
  deriving (Eq, Show)

---------------- MT Parsers ----------------

-- Parse the input string: error if parsers fail or input not consumed
progParse :: String -> Program
progParse src = case parse prog src of
  [(t, "")] -> t
  [(t, _)]  -> error "Parsing error: input not consumed"
  _         -> error "Parsing error: parsers fail"

-- Parser for MT programs
prog :: Parser Program
prog = do symbol "let"
          d <- multi decl
          symbol "in"
          c <- cmd
          return (Prog d c)

-- Parser for MT declarations
decl :: Parser Declaration
decl = do symbol "var"
          i <- identifier
          if elem i keywords then
             error "Parsing error: invalid identifiers"
          else
             do symbol ":="
                e <- expr
                return (DeclAndInit i e)
             <|>
             return (Decl i)

-- Parser for MT commands
cmd :: Parser Command
cmd = do i <- identifier
         symbol ":="
         e <- expr
         return (CmdAssign i e)
      <|>
      do symbol "if"
         e <- expr
         symbol "then"
         c1 <- cmd
         symbol "else"
         c2 <- cmd
         return (CmdIf e c1 c2)
      <|>
      do symbol "while"
         e <- expr
         symbol "do"
         c <- cmd
         return (CmdWhile e c)
      <|>
      do symbol "getint"
         i <- parens identifier
         return (CmdGet i)
      <|>
      do symbol "printint"
         e <- parens expr
         return (CmdPrint e)
      <|>
      do symbol "begin"
         c <- multi cmd
         symbol "end"
         return (CmdBegin c)

-- Converting Parser Declaration/Command to Parser Declarations/Commands
multi :: Parser a -> Parser [a]
multi p = do x <- p
             xs <- many (do symbol ";"
                            p)
             return (x:xs)

{- Parsers for MT expressions from arithExp.zip -}

expr :: Parser Expr
expr = do b <- bexp
          (do symbol "?"
              e0 <- bexp
              symbol ":"
              e1 <- bexp
              return (Conditional b e0 e1)
           <|>
           return b)

bexp :: Parser Expr
bexp = do e0 <- cexp
          (do symbol "||"
              e1 <- bexp
              return (BinOp Disjunction e0 e1)
           <|>
           return e0)

cexp :: Parser Expr
cexp = do e0 <- bterm
          (do symbol "&&"
              e1 <- cexp
              return (BinOp Conjunction e0 e1)
           <|>
           return e0)

-- Longer operators (eg "<=") must come before shorter ones ("<")
relop :: Parser BinOperator
relop = choice [ symbol "<=" >> return LeqOp
               , symbol "<"  >> return LssOp
               , symbol ">=" >> return GeqOp
               , symbol ">"  >> return GtrOp
               , symbol "==" >> return EqOp
               , symbol "!=" >> return NeqOp
               ]

bterm :: Parser Expr
bterm = do e0 <- aexp
           (do op <- relop
               e1 <- aexp
               return (BinOp op e0 e1)
            <|>
            return e0) 


addminus :: Parser BinOperator
addminus = choice [ symbol "+" >> return Addition
                  , symbol "-" >> return Subtraction
                  ]

-- For left-associativity, we use an auxiliary function aexp'
-- that keeps a functional accumulator

aexp :: Parser Expr
aexp = aexp' id

aexp' :: (Expr -> Expr) -> Parser Expr
aexp' f = do e0 <- mexp
             (do op <- addminus
                 aexp' (BinOp op (f e0))
              <|>
              return (f e0))

multdiv :: Parser BinOperator
multdiv = choice [ symbol "*" >> return Multiplication
                 , symbol "/" >> return Division
                 ]

mexp :: Parser Expr
mexp = mexp' id

mexp' :: (Expr -> Expr) -> Parser Expr
mexp' f = do e0 <- aterm
             (do op <- multdiv
                 mexp' (BinOp op (f e0))
              <|>
              return (f e0))

aterm :: Parser Expr
aterm = (natural >>= return . LitInteger)
        <|> (do i <- identifier
                return (Variable i))
        <|> (do symbol "-"
                e <- aterm
                return (UnOp Negation e))
        <|> (do symbol "!"
                b <- aterm
                return (UnOp NegBool b))
        <|> parens expr
