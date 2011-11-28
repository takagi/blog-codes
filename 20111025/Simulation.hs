module Simulation where

import Control.Monad.State
import Data.List ( nub )
import Data.Map hiding ( map )
import Prelude hiding ( lookup )


-- Definition of Exp

data Exp = Integral Exp
         | Constant Double
         | Var String
         | Add Exp Exp
         | Mul Exp Exp
  deriving (Show,Eq)

instance Num Exp where
  (+) = Add
  (-) = undefined
  (*) = Mul
  negate = undefined
  abs    = undefined
  signum = undefined
  fromInteger = undefined

integral = Integral
constant = Constant
var      = Var

checkExp :: Exp -> Bool
checkExp (Add (Constant _) (Integral exp)) = checkExpIntegralOperand exp
checkExp (Add x y)      = all (not . isIntegral) [x,y]
checkExp (Mul x y)      = all (not . isIntegral) [x,y]
checkExp (Integral exp) = checkExpIntegralOperand exp
checkExp (Constant _)   = True
checkExp (Var _)        = True

isIntegral :: Exp -> Bool
isIntegral (Integral _) = True
isIntegral otherwise    = False

checkExpIntegralOperand :: Exp -> Bool
checkExpIntegralOperand (Integral _) = False
checkExpIntegralOperand (Constant _) = True
checkExpIntegralOperand (Var _)      = True
checkExpIntegralOperand (Add x y)    = all checkExpIntegralOperand [x,y]
checkExpIntegralOperand (Mul x y)    = all checkExpIntegralOperand [x,y]

expInitialValue :: Machine -> Exp -> Double
expInitialValue _       (Add (Constant x) (Integral _)) = x
expInitialValue _       (Integral _) = 0
expInitialValue _       (Constant x) = x
expInitialValue machine (Var symbol) = expInitialValue machine
                                       $ registerExp
                                       $ machineRegister machine symbol
expInitialValue machine (Add x y)    = expInitialValue machine x
                                       + expInitialValue machine y
expInitialValue machine (Mul x y)    = expInitialValue machine x
                                       * expInitialValue machine y

expUpdatingRule :: Exp -> (Machine -> Double -> Double)
expUpdatingRule (Add (Constant _) (Integral exp)) = \machine -> (+ expUpdatingRuleIntegralOperand machine exp)
expUpdatingRule (Integral exp) = \machine -> (+ expUpdatingRuleIntegralOperand machine exp)
expUpdatingRule (Constant x)   = \_ _ -> x
expUpdatingRule (Var symbol)   = \machine _ -> machineRegisterValue machine symbol
expUpdatingRule (Add x y)      = \machine val -> (expUpdatingRule x) machine val
                                               + (expUpdatingRule y) machine val
expUpdatingRule (Mul x y)      = \machine val -> (expUpdatingRule x) machine val
                                               * (expUpdatingRule y) machine val

expUpdatingRuleIntegralOperand :: Machine -> Exp -> Double
expUpdatingRuleIntegralOperand _ (Constant x) = x
expUpdatingRuleIntegralOperand machine (Var symbol) = machineRegisterValue machine symbol
expUpdatingRuleIntegralOperand machine (Add x y) = expUpdatingRuleIntegralOperand machine x + expUpdatingRuleIntegralOperand machine y
expUpdatingRuleIntegralOperand machine (Mul x y) = expUpdatingRuleIntegralOperand machine x * expUpdatingRuleIntegralOperand machine y
expUpdatingRuleIntegralOperand _ (Integral _) = error "integral in another integral"

expUpdatingOrder :: Machine -> Exp -> [String]
expUpdatingOrder machine (Integral exp) = expUpdatingOrderIntegralOperand machine exp
expUpdatingOrder _ (Constant _)   = []
expUpdatingOrder machine (Var symbol) = expUpdatingOrder machine exp ++ [symbol]
  where exp = registerExp $ machineRegister machine symbol
expUpdatingOrder machine (Add x y) = expUpdatingOrder machine x
                                  ++ expUpdatingOrder machine y
expUpdatingOrder machine (Mul x y) = expUpdatingOrder machine x
                                  ++ expUpdatingOrder machine y

expUpdatingOrderIntegralOperand :: Machine -> Exp -> [String]
expUpdatingOrderIntegralOperand machine (Integral _) = error "integral in another integral"
expUpdatingOrderIntegralOperand machine (Constant _) = []
expUpdatingOrderIntegralOperand machine (Var symbol) =
  case exp of
    (Add (Constant _) (Integral _)) -> expUpdatingOrder machine exp ++ [symbol]
    (Integral _)                    -> expUpdatingOrder machine exp ++ [symbol]
    otherwise                       -> []
  where exp = registerExp $ machineRegister machine symbol
expUpdatingOrderIntegralOperand machine (Add x y) = 
  expUpdatingOrderIntegralOperand machine x
  ++ expUpdatingOrderIntegralOperand machine y
expUpdatingOrderIntegralOperand machine (Mul x y) = 
  expUpdatingOrderIntegralOperand machine x
  ++ expUpdatingOrderIntegralOperand machine y

expInitializingOrder :: Machine -> Exp -> [String]
expInitializingOrder _ (Integral exp) = []
expInitializingOrder _ (Constant _)   = []
expInitializingOrder machine (Var symbol) = expInitializingOrder machine exp ++ [symbol]
  where exp = registerExp $ machineRegister machine symbol
expInitializingOrder machine (Add x y) = expInitializingOrder machine x
                                      ++ expInitializingOrder machine y
expInitializingOrder machine (Mul x y) = expInitializingOrder machine x
                                      ++ expInitializingOrder machine y


-- Definition of Register

data Register = Register { registerValue        :: Double
                         , registerUpdatingRule :: Machine -> Double -> Double
                         , registerExp          :: Exp
                         }

makeRegister :: String -> Exp -> Register
makeRegister symbol exp
  | checkExp exp = Register 0 updatingRule exp
  | otherwise    = error "invalid expression"
  where updatingRule   = expUpdatingRule exp

initializeRegister :: Register -> Machine -> Register
initializeRegister reg machine = reg { registerValue = initialValue }
  where initialValue = expInitialValue machine exp
        exp          = registerExp reg

updateRegister :: Register -> Machine -> Register
updateRegister reg machine = reg { registerValue = updatingRule machine val }
  where updatingRule = registerUpdatingRule reg
        val          = registerValue reg


-- Definition of Machine

data Machine = Machine { machineRegisters         :: Map String Register
                       , machineInitializingOrder :: [String]
                       , machineUpdatingOrder     :: [String]
                       }

machineRegisterValue :: Machine -> String -> Double
machineRegisterValue machine = registerValue . machineRegister machine

machineRegister :: Machine -> String -> Register
machineRegister machine symbol = case lookup symbol regs of
                                   Just x  -> x
                                   Nothing -> error "register not found"
  where regs = machineRegisters machine

runMachine :: MachineDescription -> [Machine]
runMachine = iterate updateMachine . initializeMachine . makeMachine

makeMachine :: MachineDescription -> Machine
makeMachine desc = Machine regs initializingOrder updatingOrder
  where machine           = execState desc emptyMachine
        regs              = machineRegisters machine
        initializingOrder = _machineInitializingOrder machine
        updatingOrder     = _machineUpdatingOrder machine

emptyMachine :: Machine
emptyMachine = Machine empty (error "initializing order not set")
                             (error "updating order not set")

_machineUpdatingOrder :: Machine -> [String]
_machineUpdatingOrder = nub . concat . __machineUpdatingOrder

__machineUpdatingOrder :: Machine -> [[String]]
__machineUpdatingOrder machine = map aux (assocs regs)
  where regs = machineRegisters machine
        aux (symbol,reg) = expUpdatingOrder machine (registerExp reg)
                           ++ [symbol]

_machineInitializingOrder :: Machine -> [String]
_machineInitializingOrder = nub . concat . __machineInitializingOrder

__machineInitializingOrder :: Machine -> [[String]]
__machineInitializingOrder machine = map aux (assocs regs)
  where regs = machineRegisters machine
        aux (symbol,reg) = expInitializingOrder machine (registerExp reg)
                           ++ [symbol]

initializeMachine :: Machine -> Machine
initializeMachine machine = foldl initializeMachineRegister machine order
  where order = machineInitializingOrder machine

initializeMachineRegister :: Machine -> String -> Machine
initializeMachineRegister machine symbol = machine { machineRegisters = regs' }
  where regs  = machineRegisters machine
        regs' = insert symbol (initializeRegister reg machine) regs
                  where reg = machineRegister machine symbol

updateMachine :: Machine -> Machine
updateMachine machine = foldl updateMachineRegister machine order
  where order = machineUpdatingOrder machine


updateMachineRegister :: Machine -> String -> Machine
updateMachineRegister machine symbol = machine { machineRegisters = regs' }
  where regs  = machineRegisters machine
        regs' = insert symbol (updateRegister reg machine) regs
                  where reg = machineRegister machine symbol


-- Definition of MachineDescription

type MachineDescription = State Machine ()

define :: String -> Exp -> MachineDescription
define symbol exp = modify aux
  where aux machine = machine { machineRegisters = regs' }
                        where regs' = insert symbol reg regs
                              regs  = machineRegisters machine
                              reg   = makeRegister symbol exp

--

{-
machine1 = let machine = newMachine
               machine' = addRegister machine "x"
                                      (constant 10 + integral (constant 1))
               machine'' = addRegister machine' "v"
                                       (constant 1 + constant 2)
           in machine''
-}

{-
machine2 = do machine <- newMachine
              addMachineRegister machine "x"
                                 (constant 10 + integral (constant 1))
              addMachineRegister machine "v"
                                 (constant 1 + constant 2)
              return machine
-}

{-
machine3 = do addMachineRegister "x" (constant 10 + integral (constant 1))
              addMachineRegister "v" (constant 1 + constant 2)
-}

{-
machine3 = do allocateMachineRegister "x" (constant 10 + integral (constant 1))
              allocateMachineRegister "v" (constant 1 + constant 2)
-}

machine4 :: MachineDescription
machine4 = do define "x" (constant 10 + integral (constant 1))
              define "v" (constant 1 + constant 2)

machine5 :: MachineDescription
machine5 = do define "x" (integral (var "v" * constant dt))
              define "v" (integral (var "a" * constant dt))
              define "a" (constant g)
  where dt = 0.01
        g  = -9.8

machine6 :: MachineDescription
machine6 = do define "x" (integral (var "v" * constant dt))
              define "v" (integral (var "a" * constant dt))
              define "a" (var "f" + var "m")
              define "f" (var "x" + var "v")  -- some function that uses x and v
              define "m" (constant 1)
  where dt = 0.01

machine7 :: MachineDescription
machine7 = do define "x" (integral (var "v" * constant dt))
              define "v" (integral (var "a" * constant dt))
              define "a" (var "f" + var "m")
              define "f" (var "x" + var "v")  -- some function that uses x and v
              define "m" (constant 1)
              --
              define "q"     (integral (var "w"))
              define "w"     (integral (var "alpha"))
              define "alpha" (var "tau" + var "I")
              define "tau"   (var "f")
              define "I"     (constant 1)
  where dt = 0.01

--

{-

-- 10 + ∫1dt
reg = makeRegister ((constant 10) `add` (integral (constant 1)))

-- 1 + 2
reg2 = makeRegister ((constant 1) `add` (constant 2))

-- 10 + ∫(1+2)dt
reg3 = makeRegister ((constant 10) `add` (integral ((constant 1) `add` (constant 2))))

-- ∫∫1dtdt
reg4 = makeRegister (integral (integral (constant 1)))

-- ∫1dt + 1
reg5 = makeRegister ((integral (constant 1)) `add` (constant 1))

-}
