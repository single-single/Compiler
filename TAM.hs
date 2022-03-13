{-
Compilers (COMP3012) Coursework, 2021
  Shiqi XIN - 20125731
  (based on: Venanzio Capretta
             Nicolai Kraus)

TAM Virtual Machine.
-}

module TAM where

type MTInt = Int         -- TAM Integer type (values in stack)
type Label = String      -- TAM Label type
type Stack = [MTInt]     -- TAM stack type
type Address = Int       -- TAM address type (locations in stack)
type TAMCode = [TAMInst] -- TAM code type

-- Instructions of the Virtual Machine
data TAMInst
  = LOADL MTInt   -- push Integer into the stack
  -- Arithmetic operations
  | ADD           -- adds two top values in the stack
  | SUB           -- subtract second element of stack from top
  | MUL           -- multiplies top values in the stack
  | DIV           -- divides the second value by the top (integer division)
  | NEG           -- negates the top of the stack
  -- Boolean operations
  | AND           -- Boolean conjunction (non-zero values are True)
  | OR            -- Boolean disjunction
  | NOT           -- Boolean negation
  -- Relational operations
  | LSS           -- order operation <
  | GTR           -- order operation >
  | EQL           -- equality operator
  -- New instructions
  | HALT          -- stops execution
  | GETINT        -- reads and pushes an integer
  | PUTINT        -- pops and prints an integer
  | Label Label   -- marks a place
  | JUMP Label    -- unconditional jump
  | JUMPIFZ Label -- conditional jump
  | LOAD Address  -- reads an integer from a specified location and pushes it
  | STORE Address -- pops an integer and writes it to a specified location
  deriving (Eq,Show)

-- The empty stack
emptyStack :: Stack
emptyStack = []

-- Writing out a TAM program
writeTAM :: TAMCode -> String
writeTAM = foldl (\s inst -> s ++ show inst ++ "\n") ""

-- Parsing a TAM program
parseTAM :: String -> TAMCode
parseTAM = pTAM . words where
  pTAM ("LOADL":x:src) = LOADL (read x) : pTAM src
  pTAM ("ADD":src) = ADD : pTAM src
  pTAM ("SUB":src) = SUB : pTAM src
  pTAM ("MUL":src) = MUL : pTAM src
  pTAM ("DIV":src) = DIV : pTAM src
  pTAM ("NEG":src) = NEG : pTAM src
  pTAM ("AND":src) = AND : pTAM src
  pTAM ("OR" :src) = OR  : pTAM src
  pTAM ("NOT":src) = NOT : pTAM src
  pTAM ("LSS":src) = LSS : pTAM src
  pTAM ("GTR":src) = GTR : pTAM src
  pTAM ("EQL":src) = EQL : pTAM src
  pTAM ("HALT":src)      = HALT : pTAM src
  pTAM ("GETINT":src)    = GETINT : pTAM src
  pTAM ("PUTINT":src)    = PUTINT : pTAM src
  pTAM ("Label":l:src)   = Label l : pTAM src
  pTAM ("JUMP":l:src)    = JUMP l : pTAM src
  pTAM ("JUMPIFZ":l:src) = JUMPIFZ l : pTAM src
  pTAM ("LOAD":a:src)    = LOAD (read a) : pTAM src
  pTAM ("STORE":a:src)   = STORE (read a) : pTAM src
  pTAM _ = []
