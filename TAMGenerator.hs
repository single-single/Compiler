{-
Compilers (COMP3012) Coursework, 2021
  Shiqi XIN - 20125731
  (based on: Venanzio Capretta
             Nicolai Kraus)

Generation of TAM code from the AST of an MiniTriangle program.
-}

module TAMGenerator where

-------------- Library Files ---------------

import MiniTriangleParser
import TAM

------------- Basic Declaration ------------

type VarEnv = [(Identifier, Address)]

--------------- State Monad ----------------

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
   -- fmap :: (a -> b) -> ST a -> ST b
   fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))

instance Applicative ST where
   -- pure :: a -> ST a
   pure x = S (\s -> (x, s))

   -- (<*>) :: ST (a -> b) -> ST a -> ST b
   stf <*> stx = S (\s ->
      let (f, s')  = app stf s
          (x, s'') = app stx s' in (f x, s''))

instance Monad ST where
   -- return :: a -> ST a
   return = pure

   -- (>>=) :: ST a -> (a -> ST b) -> ST b
   st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')

--------------- TAM Generator --------------

-- State transformer for labels
freshL :: ST Label
freshL = do l <- S (\s -> (s, s + 1))
            return ("label" ++ show l)

-- Top-level function for TAM code generation
progCode :: Program -> TAMCode
progCode p = fst (app (progCodeST p) 0)

-- Generating TAM code (ST) specified by the input program
progCodeST :: Program -> ST TAMCode
progCodeST (Prog d c) = do c <- cmdCode c env
                           return (code ++ c ++ [HALT])
                           where
                             (env, code) = declsCode d

-- Initialising the variable environment and compiling declarations
declsCode :: Declarations -> (VarEnv, TAMCode)
declsCode ds = declsCode' ds ([], [])

-- Generating variable environment and code, initialised by declsCode
declsCode' :: Declarations -> (VarEnv, TAMCode) -> (VarEnv, TAMCode)
declsCode' [] vt         = vt
declsCode' (d:ds) (e, c) = declsCode' ds (e', c')
                           where
                             e' = declEnv d e
                             c' = c ++ (declInst d e')

-- Initialising the variable environment based on the input declaration
declEnv :: Declaration -> VarEnv -> VarEnv
declEnv d ve | ve == []  = [(i, 0)]
             | otherwise = ve ++ [(i, a)]
               where
                 a = (snd (last ve)) + 1
                 i = case d of
                   Decl i' -> i'
                   DeclAndInit i' _ -> i'

-- Generating TAM code based on the input declaration
declInst :: Declaration -> VarEnv -> TAMCode
declInst (Decl _) _           = [LOADL 0]
declInst (DeclAndInit i e) ve = expCode e ve

-- Generating TAM code (ST) based on the input command
cmdCode :: Command -> VarEnv -> ST TAMCode
cmdCode (CmdAssign i e) ve = return (expCode e ve ++ [STORE (getAddress i ve)])
cmdCode (CmdIf e c1 c2) ve = do l  <- freshL
                                l' <- freshL
                                i1 <- cmdCode c1 ve
                                i2 <- cmdCode c2 ve
                                return (expCode e ve ++ [JUMPIFZ l] ++ i1 ++ [JUMP l', Label l] ++ i2 ++ [Label l'])
cmdCode (CmdWhile e c) ve  = do l  <- freshL
                                l' <- freshL
                                i  <- cmdCode c ve
                                return ([Label l] ++ expCode e ve ++ [JUMPIFZ l'] ++ i ++ [JUMP l, Label l'])
cmdCode (CmdGet i)   ve = return [GETINT, STORE (getAddress i ve)]
cmdCode (CmdPrint e) ve = return (expCode e ve ++ [PUTINT])
cmdCode (CmdBegin []) _ = return []
cmdCode (CmdBegin (c:cs)) ve = do c  <- cmdCode c ve
                                  cs <- cmdCode (CmdBegin cs) ve
                                  return (c ++ cs)

-- Generating TAM code based on the input expression
-- (Modified from the function in arithExp.zip）
expCode :: Expr -> VarEnv -> TAMCode
expCode (LitInteger x) ve = [LOADL x]
expCode (Variable i)   ve = [LOAD x]
                            where x = getAddress i ve
-- Relational operators that don't have TAM instructions
expCode (BinOp LeqOp t1 t2) ve =
  expCode (BinOp Disjunction (BinOp LssOp t1 t2) (BinOp EqOp t1 t2)) ve
expCode (BinOp GeqOp t1 t2) ve =
  expCode (BinOp Disjunction (BinOp GtrOp t1 t2) (BinOp EqOp t1 t2)) ve
expCode (BinOp NeqOp t1 t2) ve =
  expCode (UnOp NegBool (BinOp EqOp t1 t2)) ve
-- Conditional expressions (double negation to normalize the Boolean value)
--   b ? e1 : e2  ~  (!!b) * e1 + (!b) * e2
expCode (Conditional b t1 t2) ve =
  expCode (BinOp Addition
             (BinOp Multiplication (UnOp NegBool (UnOp NegBool b)) t1)
             (BinOp Multiplication (UnOp NegBool b) t2)) ve
-- General cases
expCode (BinOp op t1 t2) ve = (expCode t1 ve) ++ (expCode t2 ve) ++ [binOpTAM op]
expCode (UnOp op t)      ve = (expCode t  ve) ++ [unOpTAM op]

-- Retrieving the address of the input identifier in the stack
getAddress :: Identifier -> VarEnv -> Address
getAddress i ve = case lookup i ve of
                    Nothing -> error "Parsing error: variable not in scope"
                    Just a  -> a

{- Code for compiling arithmetic operations from arithExp.zip -}

binOpTAM :: BinOperator -> TAMInst
binOpTAM Addition       = ADD
binOpTAM Subtraction    = SUB
binOpTAM Multiplication = MUL
binOpTAM Division       = DIV
binOpTAM Conjunction    = AND
binOpTAM Disjunction    = OR
binOpTAM LssOp          = LSS
binOpTAM GtrOp          = GTR
binOpTAM EqOp           = EQL

unOpTAM :: UnOperator -> TAMInst
unOpTAM Negation = NEG
unOpTAM NegBool  = NOT
