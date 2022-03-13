{-
Compilers (COMP3012) Coursework, 2021
  Shiqi XIN - 20125731
  (based on: Venanzio Capretta
             Nicolai Kraus)

Executor for TAM.
-}

module TAMExecutor where

-------------- Library Files ---------------

import TAM
import MiniTriangleParser

import System.IO
import Data.Char
import Data.List

------------- Basic Definitions ------------

type Counter = Int

data TAMState = TAMState {
  tsCode :: TAMCode,
  tsCounter :: Counter,
  tsStack :: Stack
} deriving (Eq, Show)

-------------- StateIO Monad ---------------

newtype TAMSt a = StT (TAMState -> IO (a, TAMState))

appT :: TAMSt a -> TAMState -> IO (a, TAMState)
appT (StT st) x = st x

lift :: IO a -> TAMSt a
lift m = StT (\s -> do x <- m
                       return (x, s))

instance Functor TAMSt where
  -- fmap :: (a -> b) -> TAMSt a -> TAMSt b
  fmap g st = StT (\s -> do (x, s') <- appT st s
                            return (g x, s'))

instance Applicative TAMSt where
  -- pure :: a -> TAMSt a
  pure x = StT (\s -> do return (x, s))

  -- (<*>) :: TAMSt (a -> b) -> TAMSt a -> TAMSt b
  stf <*> stx = StT (\s -> do (f, s')  <- appT stf s
                              (x, s'') <- appT stx s'
                              return (f x, s''))

instance Monad TAMSt where
  -- return :: a -> TAMSt a
  return = pure

  -- (>>=) :: TAMSt a -> (a -> TAMSt b) -> TAMSt b
  st >>= f = StT (\s -> do (x, s') <- appT st s
                           appT (f x) s')

--------------- TAM Executor ---------------

-- Executing a TAM program (list of instructions)
execTAM :: TAMCode -> IO Stack
execTAM c = do (s, _) <- appT tsProceed (tsInit c)
               return s

-- Effect of a single operation on the stack
execute :: TAMInst -> TAMSt Stack
-- arithmetic operators
execute ADD = tsBinOp (+)
execute SUB = tsBinOp (-)
execute MUL = tsBinOp (*)
execute DIV = tsBinOp div
execute NEG = tsUnOp (0-)
-- Boolean operators
execute AND = tsBinOp intAND
execute OR  = tsBinOp intOR
execute NOT = tsUnOp intNOT
-- relational operators
execute LSS = tsBinOp intLSS
execute GTR = tsBinOp intGTR
execute EQL = tsBinOp intEQL
-- new instructions
execute HALT   = stackTs
execute GETINT = do lift (putStr "Please enter a number: ")
                    lift (hFlush stdout)
                    i <- lift getNum
                    tsPush i
                    tsNext
execute PUTINT = do n <- tsPop
                    lift (putStrLn ("Output: " ++ show n))
                    tsNext
execute (Label l)   = do tsNext
execute (JUMP l)    = do c <- tsLCounter l
                         tsSetCounter c
                         tsProceed
execute (JUMPIFZ l) = do n <- tsPop
                         if n == 0 then
                           execute (JUMP l)
                         else
                           tsNext
execute (LOAD a)  = do n <- tsReadStack a
                       tsPush n
                       tsNext
execute (STORE a) = do n <- tsPop
                       tsWriteStack a n
                       tsNext
execute (LOADL n) = do tsPush n
                       tsNext

-- Retrieving the state of the code
codeTs :: TAMSt TAMCode
codeTs = StT (\s -> return (tsCode s, s))

-- Retrieving the state of the counter
counterTs :: TAMSt Counter
counterTs = StT (\s -> return (tsCounter s, s))

-- Retrieving the state of the stack
stackTs :: TAMSt Stack
stackTs = StT (\s -> return (tsStack s, s))

-- Initialising the TAM state
tsInit :: TAMCode -> TAMState
tsInit c = TAMState { tsCode = c, tsCounter = 0, tsStack = [] }

-- Pushing an item to the top of the stack
tsPush :: MTInt -> TAMSt ()
tsPush n = StT (\s -> do let ns = tsStack s
                         return ((), s { tsStack = n:ns }))

-- Popping the top element from the stack
tsPop :: TAMSt MTInt
tsPop = StT (\s -> do let (n:ns) = tsStack s
                      return (n, s { tsStack = ns }))

-- Retrieving the element at the specified address of the stack
tsReadStack :: Address -> TAMSt MTInt
tsReadStack a = do s <- stackTs
                   let i = length s - a - 1
                   return (s !! i)

-- Adding an element to the specified address of the stack
tsWriteStack :: Address -> MTInt -> TAMSt ()
tsWriteStack a i = do xs <- stackTs
                      let n = (length xs) - a - 1
                      let stack = take n xs ++ [i] ++ drop (n + 1) xs
                      StT (\s -> return ((), s { tsStack = stack }))

-- Getting the location of the specified label in the stack
tsLCounter :: Label -> TAMSt Counter
tsLCounter l = do c <- codeTs
                  let xs = fst (break (== Label l) c)
                  return (length xs)

-- Updating the counter
tsSetCounter :: Counter -> TAMSt ()
tsSetCounter c = StT (\s -> return ((), s { tsCounter = c }))

-- Performing a binary operation in the stack
tsBinOp :: (MTInt -> MTInt -> MTInt) -> TAMSt Stack
tsBinOp op = do x <- tsPop
                y <- tsPop
                tsPush (op y x)
                tsNext

-- Performing a unary operation in the stack
tsUnOp :: (MTInt -> MTInt) -> TAMSt Stack
tsUnOp op = do x <- tsPop
               tsPush (op x)
               tsNext

-- Executing the next (sequential) instruction
tsNext :: TAMSt Stack
tsNext = do c <- counterTs
            tsSetCounter (c + 1)
            tsProceed

-- Executing the next (not necessarily sequential) instruction
tsProceed :: TAMSt Stack
tsProceed = do code <- codeTs
               counter <- counterTs
               let i = code !! counter
               execute i

-- Read a number: ask to re-enter if the input is invalid
getNum :: IO Int
getNum = do xs <- getLine
            if xs /= "" && all isDigit xs then
               return (read xs)
            else
              do putStr "Invalid input, please try again: "
                 hFlush stdout
                 getNum

{- Code for carrying out arithmetic operations from arithExp.zip -}

-- Correspondence between Booleans and integers
boolInt :: Bool -> MTInt
boolInt False = 0
boolInt True = 1

-- All non-zero integers correspond to Boolean false
intBool :: MTInt -> Bool
intBool x = x/=0

-- Convenient composition operators

-- Pre-composing with a 2-argument function
infixr 9 .<
(.<) :: (b -> c) -> (a -> a -> b) -> a -> a -> c
g .< f = \ a1 a2 -> g (f a1 a2)

-- Post-composing with a 2-argument function
infixr 9 <.
(<.) :: (b -> b -> c) -> (a -> b) -> a -> a -> c
g <. f = \ a1 a2 -> g (f a1) (f a2)

-- Implementation of boolean operations on Integers, always return 0 or 1

intAND :: MTInt -> MTInt -> MTInt
intAND = boolInt .< (&&) <. intBool

intOR :: MTInt -> MTInt -> MTInt
intOR = boolInt .< (||) <. intBool

intNOT :: MTInt -> MTInt
intNOT = boolInt . not . intBool

-- Relational operations, return 0 (False) or 1 (True)

intLSS :: MTInt -> MTInt -> MTInt
intLSS = boolInt .< (<)

intGTR :: MTInt -> MTInt -> MTInt
intGTR = boolInt .< (>)

intEQL :: MTInt -> MTInt -> MTInt
intEQL = boolInt .< (==)
