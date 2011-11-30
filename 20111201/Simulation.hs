{-# LANGUAGE TypeSynonymInstances #-}

import Control.Monad.State hiding ( join )
import Data.List ( nub )
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Maybe



-- Definition of SimValue

type Scalar = Double

type Vector3 = ( Scalar, Scalar, Scalar )

addVector :: Vector3 -> Vector3 -> Vector3
addVector (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)

subVector :: Vector3 -> Vector3 -> Vector3
subVector (x0,y0,z0) (x1,y1,z1) = (x0-x1,y0-y1,z0-z1)

scaleVector :: Scalar -> Vector3 -> Vector3
scaleVector a (x,y,z) = (a*x, a*y, a*z)

normVector :: Vector3 -> Scalar
normVector (x,y,z) = sqrt $ x*x + y*y + z*z

type Array = V.Vector


-- Definition of RegisterValueOne

data RegisterValueOne = RegisterValueOneScalar Scalar
                      | RegisterValueOneVector Vector3
  deriving (Show, Eq)

addRegisterValueOne :: RegisterValueOne -> RegisterValueOne -> RegisterValueOne
addRegisterValueOne (RegisterValueOneScalar x) (RegisterValueOneScalar y)
  = RegisterValueOneScalar $ x + y
addRegisterValueOne (RegisterValueOneVector x) (RegisterValueOneVector y)
  = RegisterValueOneVector $ addVector x y
addRegisterValueOne _ _
  = error "addRegisterValueOne: invalid operand(s)"

subRegisterValueOne :: RegisterValueOne -> RegisterValueOne -> RegisterValueOne
subRegisterValueOne (RegisterValueOneScalar x) (RegisterValueOneScalar y)
  = RegisterValueOneScalar $ x - y
subRegisterValueOne (RegisterValueOneVector x) (RegisterValueOneVector y)
  = RegisterValueOneVector $ subVector x y
subRegisterValueOne _ _
  = error "subRegisterValueOne: invalid operand(s)"

mulRegisterValueOne :: RegisterValueOne -> RegisterValueOne -> RegisterValueOne
mulRegisterValueOne (RegisterValueOneScalar x) (RegisterValueOneScalar y)
  = RegisterValueOneScalar $ x * y
mulRegisterValueOne (RegisterValueOneScalar a) (RegisterValueOneVector x)
  = RegisterValueOneVector $ scaleVector a x
mulRegisterValueOne (RegisterValueOneVector x) (RegisterValueOneScalar a)
  = RegisterValueOneVector $ scaleVector a x
mulRegisterValueOne _ _ = error "mulRegisterValueOne: invalid operand(s)"

divRegisterValueOne :: RegisterValueOne -> RegisterValueOne -> RegisterValueOne
divRegisterValueOne (RegisterValueOneScalar x) (RegisterValueOneScalar y)
  = RegisterValueOneScalar $ x / y
divRegisterValueOne (RegisterValueOneVector x) (RegisterValueOneScalar a)
  = RegisterValueOneVector $ scaleVector (recip a) x
divRegisterValueOne _ _ = error "divRegisterValueOne: invalid operand(s)"

normRegisterValueOne :: RegisterValueOne -> RegisterValueOne
normRegisterValueOne (RegisterValueOneVector x)
  = RegisterValueOneScalar $ normVector x
normRegisterValueOne _ = error "normRegisterValueOne: invalid operand"


-- Definition of RegisterValue

data RegisterValue = RegisterValueScalar (Array Scalar)
                   | RegisterValueVector (Array Vector3)

instance Show RegisterValue where
  show (RegisterValueScalar x) = show $ V.toList x
  show (RegisterValueVector x) = show $ V.toList x

registerValueZeroS :: Int -> RegisterValue
registerValueZeroS = RegisterValueScalar . flip V.replicate 0

registerValueZeroV :: Int -> RegisterValue
registerValueZeroV = RegisterValueVector . flip V.replicate (0,0,0)

makeRegisterValueWithS :: Int -> (Int -> RegisterValueOne) -> RegisterValue
makeRegisterValueWithS n fn = RegisterValueScalar $ V.generate n $ \i ->
                                case fn i of
                                  RegisterValueOneScalar x -> x
                                  otherwise                -> error msg
  where msg = "makeRegisterValueWithS: invalid value"

makeRegisterValueWithV :: Int -> (Int -> RegisterValueOne) -> RegisterValue
makeRegisterValueWithV n fn = RegisterValueVector $ V.generate n $ \i ->
                                case fn i of
                                  RegisterValueOneVector x -> x
                                  otherwise                -> error msg
  where msg = "makeRegisterValueWithV: invalid value"

registerValueAt :: RegisterValue -> Int -> RegisterValueOne
registerValueAt val i = case val of
  RegisterValueScalar x -> RegisterValueOneScalar ((V.!) x i)
  RegisterValueVector x -> RegisterValueOneVector ((V.!) x i)

addRegisterValue :: RegisterValue -> RegisterValue -> RegisterValue
addRegisterValue (RegisterValueScalar x) (RegisterValueScalar y)
  = RegisterValueScalar $ V.zipWith (+) x y
addRegisterValue (RegisterValueVector x) (RegisterValueVector y)
  = RegisterValueVector $ V.zipWith addVector x y
addRegisterValue _ _
  = error "addRegisterValue: invalid operand(s)"

subRegisterValue :: RegisterValue -> RegisterValue -> RegisterValue
subRegisterValue (RegisterValueScalar x) (RegisterValueScalar y)
  = RegisterValueScalar $ V.zipWith (-) x y
subRegisterValue (RegisterValueVector x) (RegisterValueVector y)
  = RegisterValueVector $ V.zipWith subVector x y
subRegisterValue _ _
  = error "subRegisterValue: invalid operand(s)"

mulRegisterValue :: RegisterValue -> RegisterValue -> RegisterValue
mulRegisterValue (RegisterValueScalar x) (RegisterValueScalar y)
  = RegisterValueScalar $ V.zipWith (*) x y
mulRegisterValue (RegisterValueScalar a) (RegisterValueVector x)
  = RegisterValueVector $ V.zipWith scaleVector a x
mulRegisterValue (RegisterValueVector x) (RegisterValueScalar a)
  = RegisterValueVector $ V.zipWith scaleVector a x
mulRegisterValue _ _
  = error "mulRegisterValue: invalid operand(s)"

divRegisterValue :: RegisterValue -> RegisterValue -> RegisterValue
divRegisterValue (RegisterValueScalar x) (RegisterValueScalar y)
  = RegisterValueScalar $ V.zipWith (/) x y
divRegisterValue (RegisterValueVector x) (RegisterValueScalar a)
  = RegisterValueVector $ V.zipWith scaleVector a' x
      where a' = V.map (\x -> 1 / x) a
divRegisterValue _ _
  = error "divRegisterValue: invalid operand(s)"

normRegisterValue :: RegisterValue -> RegisterValue
normRegisterValue (RegisterValueVector x)
  = RegisterValueScalar $ V.map normVector x
normRegisterValue _
  = error "normRegisterValue: invalid operand"


-- Definition of Register

data Register = Register { registerSymbol       :: String
                         , registerValue        :: RegisterValue
                         , registerUpdatingRule :: RegisterUpdatingRule
                         }

type RegisterUpdatingRule = Machine -> RegisterValue -> RegisterValue

makeRegister :: String -> RegisterValue -> RegisterUpdatingRule -> Register
makeRegister = Register

updateRegister :: Register -> Machine -> Register
updateRegister reg machine = reg { registerValue = update machine val }
  where update = registerUpdatingRule reg
        val    = registerValue reg


-- Definition of Machine

data Machine = Machine { machineRegisters          :: MachineRegisters
                       , machineRegisterDependency :: [String]
                       }

type MachineRegisters = M.Map String Register

makeMachine :: Machine
makeMachine = Machine M.empty []

addMachineRegister :: String -> RegisterValue -> RegisterUpdatingRule -> Machine -> Machine
addMachineRegister symbol val fn machine = machine { machineRegisters = regs' }
  where regs' = M.insert symbol (makeRegister symbol val fn) regs
        regs = machineRegisters machine

setMachineRegisterDependency :: [String] -> Machine -> Machine
setMachineRegisterDependency deps machine
  = machine { machineRegisterDependency = deps }

machineRegister :: Machine -> String -> Register
machineRegister machine symbol
  = case M.lookup symbol regs of
      Just x  -> x
      Nothing -> error ("register not found: " ++ symbol)
  where regs = machineRegisters machine

machineRegisterValue :: Machine -> String -> RegisterValue
machineRegisterValue machine = registerValue . machineRegister machine

machineRegisterValueV :: Machine -> String -> Array Vector3
machineRegisterValueV machine symbol
  = case registerValue (machineRegister machine symbol) of
      RegisterValueVector x -> x
      otherwise             -> error "machineRegisterValueV: invalid type"

machineRegisterValueOne :: Machine -> String -> Int -> RegisterValueOne
machineRegisterValueOne machine symbol i = registerValueAt val i
  where val = machineRegisterValue machine symbol

runMachine :: Machine -> [Machine]
runMachine = iterate updateMachine
           . checkMachineRegisterDependency

checkMachineRegisterDependency :: Machine -> Machine
checkMachineRegisterDependency machine
  | null deps = error "invalid machine register dependency"
  | otherwise = machine
  where deps = machineRegisterDependency machine

updateMachine :: Machine -> Machine
updateMachine machine = foldl updateMachineRegister machine order
  where order = machineRegisterDependency machine

updateMachineRegister :: Machine -> String -> Machine
updateMachineRegister machine symbol = machine { machineRegisters = regs' }
  where regs' = let regs = machineRegisters machine
                    reg  = machineRegister machine symbol
                in M.insert symbol (updateRegister reg machine) regs


-- Definition of Machine specialized for simulation

makeSimMachine :: Equations -> Machine
makeSimMachine eqs = setSimMachineRegisterDependency eqs
                   $ setSimMachineRegisters eqs
                   $ makeMachine

setSimMachineRegisters eqs machine = foldl aux machine (equations eqs)
  where aux :: Machine -> (String,Equation) -> Machine
        aux machine (symbol,eq) = addMachineRegister symbol val fn machine
          where val = initialValue eqs symbol eq
                fn  = updatingRule eqs symbol (equationExp eq)

initialValue :: Equations -> String -> Equation -> RegisterValue
initialValue eqs s0 eq
  | isIntegral (equationExp eq) = case equationsInitialCondition eqs symbol of
                                    Just x  -> registerValueFromInitialCondition x
                                    Nothing -> initialValueExp eqs s0 symbol exp
  | otherwise                   = initialValueExp eqs s0 symbol exp
  where symbol = equationSymbol eq
        exp    = equationExp eq


initialValueExp :: Equations -> String -> String -> Exp -> RegisterValue
initialValueExp eqs _  s1 (Integral _)
  = let n = equationsArraySize eqs
    in case equationType (equation eqs s1) of
      ExpTypeScalar -> registerValueZeroS n
      ExpTypeVector -> registerValueZeroV n
initialValueExp eqs _  _  (Constant x)
  = registerValueFromExpValue (equationsArraySize eqs) x
initialValueExp eqs s0 _  (Var symbol)
  | symbol == s0 = error "initialValueExp: circular reference"
  | otherwise    = initialValue eqs s0 (equation eqs symbol)
initialValueExp _   _  _  (Var' _)
  = error "initialValueExp: Var' outside Sigma"
initialValueExp eqs s0 s1 (Add x y)
  = addRegisterValue (initialValueExp eqs s0 s1 x)
                     (initialValueExp eqs s0 s1 y)
initialValueExp eqs s0 s1 (Sub x y)
  = subRegisterValue (initialValueExp eqs s0 s1 x)
                     (initialValueExp eqs s0 s1 y)
initialValueExp eqs s0 s1 (Mul x y)
  = mulRegisterValue (initialValueExp eqs s0 s1 x)
                     (initialValueExp eqs s0 s1 y)
initialValueExp eqs s0 s1 (Div x y)
  = divRegisterValue (initialValueExp eqs s0 s1 x)
                     (initialValueExp eqs s0 s1 y)
initialValueExp eqs s0 s1 (Norm x)
  = normRegisterValue (initialValueExp eqs s0 s1 x)
initialValueExp eqs s0 s1 (Sigma exp)
  = let typ = equationType (equation eqs s1)
        n   = equationsArraySize eqs
        makeRegisterValueWith = case typ of
          ExpTypeScalar -> makeRegisterValueWithS
          ExpTypeVector -> makeRegisterValueWithV
    in makeRegisterValueWith n $ \i ->
         let is = filter (/= i) [0..n-1]
         in (foldl1 addRegisterValueOne $ flip map is $ \j ->
              initialValueSigmaOperand eqs s0 exp i j)

initialValueSigmaOperand :: Equations -> String -> Exp -> Int -> Int -> RegisterValueOne
initialValueSigmaOperand _ _ (Integral _) _ _
  = error "initialValueSigmaOperand: integral in sigma"
initialValueSigmaOperand _ _ (Constant x) _ _
  = registerValueOneFromExpValue x
initialValueSigmaOperand eqs s0 (Var symbol) i _
  = let eq = equation eqs symbol
    in registerValueAt (initialValue eqs s0 eq) i
initialValueSigmaOperand eqs s0 (Var' symbol) _ j
  = let eq = equation eqs symbol
    in registerValueAt (initialValue eqs s0 eq) j
initialValueSigmaOperand eqs s0 (Add x y) i j
  = addRegisterValueOne (initialValueSigmaOperand eqs s0 x i j)
                        (initialValueSigmaOperand eqs s0 y i j)
initialValueSigmaOperand eqs s0 (Sub x y) i j
  = subRegisterValueOne (initialValueSigmaOperand eqs s0 x i j)
                        (initialValueSigmaOperand eqs s0 y i j)
initialValueSigmaOperand eqs s0 (Mul x y) i j
  = mulRegisterValueOne (initialValueSigmaOperand eqs s0 x i j)
                        (initialValueSigmaOperand eqs s0 y i j)
initialValueSigmaOperand eqs s0 (Div x y) i j
  = divRegisterValueOne (initialValueSigmaOperand eqs s0 x i j)
                        (initialValueSigmaOperand eqs s0 y i j)
initialValueSigmaOperand eqs s0 (Norm x) i j
  = normRegisterValueOne (initialValueSigmaOperand eqs s0 x i j)
initialValueSigmaOperand _ _ (Sigma _) _ _
  = error "initialValueSigmaOperand: sigma in another sigma"

updatingRule :: Equations -> String -> Exp -> (Machine -> RegisterValue -> RegisterValue)
updatingRule eqs symbol (Integral exp)
  = \machine val -> addRegisterValue
                      val ((updatingRule eqs symbol exp) machine val)
updatingRule eqs _      (Constant x)
  = \_       _   -> registerValueFromExpValue n x
                      where n = equationsArraySize eqs
updatingRule eqs _      (Var symbol)
  = \machine _   -> machineRegisterValue machine symbol
updatingRule eqs symbol (Add x y)
  = \machine val -> addRegisterValue ((updatingRule eqs symbol x) machine val)
                                     ((updatingRule eqs symbol y) machine val)
updatingRule eqs symbol (Sub x y)
  = \machine val -> subRegisterValue ((updatingRule eqs symbol x) machine val)
                                     ((updatingRule eqs symbol y) machine val)
updatingRule eqs symbol (Mul x y)
  = \machine val -> mulRegisterValue ((updatingRule eqs symbol x) machine val)
                                     ((updatingRule eqs symbol y) machine val)
updatingRule eqs symbol (Div x y)
  = \machine val -> divRegisterValue ((updatingRule eqs symbol x) machine val)
                                     ((updatingRule eqs symbol y) machine val)
updatingRule eqs symbol (Norm x)
  = \machine val -> normRegisterValue ((updatingRule eqs symbol x) machine val)
updatingRule eqs symbol (Sigma exp)
  = let makeRegisterValueWith = case equationType (equation eqs symbol) of
          ExpTypeScalar -> makeRegisterValueWithS
          ExpTypeVector -> makeRegisterValueWithV
        n = equationsArraySize eqs
    in \machine _ -> makeRegisterValueWith n $ \i ->
                       let is = filter (/= i) [0..n-1]
                       in (foldl1 addRegisterValueOne $ flip map is $ \j ->
                            (updatingRuleSigmaOperand exp) machine i j)

updatingRuleSigmaOperand :: Exp -> (Machine -> Int -> Int -> RegisterValueOne)
updatingRuleSigmaOperand (Integral _) = error "updatingRuleSigmaOperand: must not be reached"
updatingRuleSigmaOperand (Constant x)
  = \_ _ _ -> registerValueOneFromExpValue x
updatingRuleSigmaOperand (Var symbol)
  = \machine i _ -> machineRegisterValueOne machine symbol i
updatingRuleSigmaOperand (Var' symbol)
  = \machine _ j -> machineRegisterValueOne machine symbol j
updatingRuleSigmaOperand (Add x y)
  = \machine i j -> addRegisterValueOne
                      ((updatingRuleSigmaOperand x) machine i j)
                      ((updatingRuleSigmaOperand y) machine i j)
updatingRuleSigmaOperand (Sub x y)
  = \machine i j -> subRegisterValueOne
                      ((updatingRuleSigmaOperand x) machine i j)
                      ((updatingRuleSigmaOperand y) machine i j)
updatingRuleSigmaOperand (Mul x y)
  = \machine i j -> mulRegisterValueOne
                      ((updatingRuleSigmaOperand x) machine i j)
                      ((updatingRuleSigmaOperand y) machine i j)
updatingRuleSigmaOperand (Div x y)
  = \machine i j -> divRegisterValueOne
                      ((updatingRuleSigmaOperand x) machine i j)
                      ((updatingRuleSigmaOperand y) machine i j)
updatingRuleSigmaOperand (Norm x)
  = \machine i j -> normRegisterValueOne
                      ((updatingRuleSigmaOperand x) machine i j)
updatingRuleSigmaOperand (Sigma _) = error "must not be reached"

setSimMachineRegisterDependency :: Equations -> Machine -> Machine
setSimMachineRegisterDependency = setMachineRegisterDependency
                                . equationsDependency

runSimMachine :: EquationsDescription -> [Machine]
runSimMachine = runMachine
              . makeSimMachine
              . checkEquations
              . inferEquations
              . flip execState emptyUninferredEquations

registerValueOneFromExpValue :: ExpValue -> RegisterValueOne
registerValueOneFromExpValue (ExpValueScalar x) = RegisterValueOneScalar x
registerValueOneFromExpValue (ExpValueVector x) = RegisterValueOneVector x

registerValueFromExpValue :: Int -> ExpValue -> RegisterValue
registerValueFromExpValue n (ExpValueScalar x)
  = RegisterValueScalar (V.replicate n x)
registerValueFromExpValue n (ExpValueVector x)
  = RegisterValueVector (V.replicate n x)

registerValueFromInitialCondition :: InitialCondition -> RegisterValue
registerValueFromInitialCondition (InitialConditionScalar x)
  = RegisterValueScalar x
registerValueFromInitialCondition (InitialConditionVector x)
  = RegisterValueVector x


-- Definition of EquationsDescription

type EquationsDescription = State UninferredEquations ()

define :: String -> Exp -> EquationsDescription
define symbol exp = modify aux
  where aux eqs = addUninferredEquation eqs symbol Nothing exp

defineWithType :: String -> Exp -> ExpType -> EquationsDescription
defineWithType symbol exp t = modify aux
  where aux eqs = addUninferredEquation eqs symbol (Just t) exp

initialConditionS :: String -> Array Scalar -> EquationsDescription
initialConditionS symbol xs = modify aux
  where aux eqs = addInitialCondition eqs symbol (InitialConditionScalar xs)

initialConditionV :: String -> Array Vector3 -> EquationsDescription
initialConditionV symbol xs = modify aux
  where aux eqs = addInitialCondition eqs symbol (InitialConditionVector xs)


-- Definition of UninferredEquations

data UninferredEquations = UninferredEquations (M.Map String UninferredEquation)
                                               (M.Map String InitialCondition)

emptyUninferredEquations :: UninferredEquations
emptyUninferredEquations = UninferredEquations M.empty M.empty

uninferredEquation :: UninferredEquations -> String -> UninferredEquation
uninferredEquation (UninferredEquations x _) symbol
  = case M.lookup symbol x of
      Just x  -> x
      Nothing -> error "uninferredEquation: symbol not found"

addUninferredEquation :: UninferredEquations -> String -> Maybe ExpType -> Exp -> UninferredEquations
addUninferredEquation (UninferredEquations eqs ics) symbol t exp
  = UninferredEquations eqs' ics
    where eqs' = M.insert symbol (makeUninferredEquation symbol t exp) eqs

addInitialCondition :: UninferredEquations -> String -> InitialCondition -> UninferredEquations
addInitialCondition (UninferredEquations eqs ics) symbol ic
  = UninferredEquations eqs ics'
    where ics' = M.insert symbol ic ics

inferEquations :: UninferredEquations -> Equations
inferEquations eqs@(UninferredEquations xs ys) = Equations xs' ys
  where xs' = M.map (inferEquation eqs) xs


-- Definition of UninferredEquation

data UninferredEquation = UninferredEquation
  { uninferredEquationSymbol :: String
  , uninferredEquationType   :: (Maybe ExpType)
  , uninferredEquationExp    :: Exp
  }

makeUninferredEquation :: String -> Maybe ExpType -> Exp -> UninferredEquation
makeUninferredEquation = UninferredEquation

inferEquation :: UninferredEquations -> UninferredEquation -> Equation
inferEquation eqs (UninferredEquation symbol t exp) = makeEquation symbol t' exp
  where t' = case t of
               Just x  -> x
               Nothing -> inferExpType eqs symbol exp

inferExpType :: UninferredEquations -> String -> Exp -> ExpType
inferExpType eqs s0 (Integral exp)                = inferExpType eqs s0 exp
inferExpType _   _  (Constant (ExpValueScalar _)) = ExpTypeScalar
inferExpType _   _  (Constant (ExpValueVector _)) = ExpTypeVector
inferExpType eqs s0 (Var symbol)
  | isJust t     = fromJust t
  | symbol == s0 = error "inferExpType: circular reference"
  | otherwise    = inferExpType eqs s0 exp
  where t   = uninferredEquationType $ uninferredEquation eqs symbol
        exp = uninferredEquationExp $ uninferredEquation eqs symbol
inferExpType eqs s0 (Var' symbol)
  | isJust t     = fromJust t
  | symbol == s0 = error "inferExpType: circular reference"
  | otherwise    = inferExpType eqs s0 exp
  where t   = uninferredEquationType $ uninferredEquation eqs symbol
        exp = uninferredEquationExp $ uninferredEquation eqs symbol
inferExpType eqs s0 (Add x y)
  = case (inferExpType eqs s0 x, inferExpType eqs s0 y) of
      (ExpTypeScalar, ExpTypeScalar) -> ExpTypeScalar
      (ExpTypeVector, ExpTypeVector) -> ExpTypeVector
      otherwise                      -> error "inferExpType: invalid operand(s)"
inferExpType eqs s0 (Sub x y)
  = case (inferExpType eqs s0 x, inferExpType eqs s0 y) of
      (ExpTypeScalar, ExpTypeScalar) -> ExpTypeScalar
      (ExpTypeVector, ExpTypeVector) -> ExpTypeVector
      otherwise                      -> error "inferExpType: invalid operand(s)"
inferExpType eqs s0 (Mul x y)
  = case (inferExpType eqs s0 x, inferExpType eqs s0 y) of
      (ExpTypeScalar, ExpTypeScalar) -> ExpTypeScalar
      (ExpTypeScalar, ExpTypeVector) -> ExpTypeVector
      (ExpTypeVector, ExpTypeScalar) -> ExpTypeVector
      otherwise                      -> error "inferExpType: invalid operand(s)"
inferExpType eqs s0 (Div x y)
  = case (inferExpType eqs s0 x, inferExpType eqs s0 y) of
      (ExpTypeScalar, ExpTypeScalar) -> ExpTypeScalar
      (ExpTypeVector, ExpTypeScalar) -> ExpTypeVector
      otherwise                      -> error "inferExpType: invalid operand(s)"
inferExpType eqs s0 (Norm x)
  = case inferExpType eqs s0 x of
      ExpTypeVector -> ExpTypeScalar
      otherwise     -> error "inferExpType: invalid operand"
inferExpType eqs s0 (Sigma exp) = inferExpType eqs s0 exp


-- Definition of Equations

data Equations = Equations (M.Map String Equation)
                           (M.Map String InitialCondition)

instance Show Equations where
 show eqs = unlines (["definition(s):"] ++
                     (if not $ null $ equations eqs
                      then map (indent . show . snd) (equations eqs)
                      else [indent "no definitions"])
                     ++ [indent ""] ++
                     ["initial condition(s):"] ++
                     (if (not $ null $ equationsInitialConditions eqs)
                      then map (indent . show . snd)
                               (equationsInitialConditions eqs)
                      else [indent "no initial conditions"]))
               where indent = (++) "  "

emptyEquations :: Equations
emptyEquations = Equations M.empty M.empty

equation :: Equations -> String -> Equation
equation (Equations eqs _) symbol
  = case M.lookup symbol eqs of
      Just x  -> x
      Nothing -> error "equation: symbol not found"

equations :: Equations -> [(String,Equation)]
equations (Equations x _) = M.assocs x

equationsDependency :: Equations -> [String]
equationsDependency eqs = nub $ concat $ map aux $ equations eqs
  where aux (symbol,eq) = dependency eqs symbol (equationExp eq) ++ [symbol]

equationsInitialConditions :: Equations -> [(String,InitialCondition)]
equationsInitialConditions (Equations _ x) = M.assocs x

equationsInitialCondition :: Equations -> String -> Maybe InitialCondition
equationsInitialCondition (Equations _ x) symbol = M.lookup symbol x

equationsArraySize :: Equations -> Int
equationsArraySize eqs = initialConditionArraySize
                       $ snd $ head $ equationsInitialConditions
                       $ checkEquationsArraySize eqs

checkEquationsArraySize :: Equations -> Equations
checkEquationsArraySize eqs
  = case n of
      1         -> eqs
      0         -> error "equationsArraySize: array size undefined"
      otherwise -> error "equationsArraySize: invalid array size"
  where n = length
          $ nub 
          $ map initialConditionArraySize 
          $ map snd 
          $ equationsInitialConditions eqs

dependency :: Equations -> String -> Exp -> [String]
dependency eqs _  (Integral exp) = dependencyOfIntegralOperand eqs exp
dependency _   _  (Constant _)   = []
dependency eqs s0 (Var symbol)
  | s0 == symbol = error "dependency: circular reference"
  | otherwise    = dependency eqs s0 exp ++ [symbol]
  where exp = equationExp $ equation eqs symbol
dependency eqs s0 (Var' symbol)
  | s0 == symbol = error "dependency: circular reference"
  | otherwise    = dependency eqs s0 exp ++ [symbol]
  where exp = equationExp $ equation eqs symbol
dependency eqs s0 (Add x y)   = dependency eqs s0 x ++ dependency eqs s0 y
dependency eqs s0 (Sub x y)   = dependency eqs s0 x ++ dependency eqs s0 y
dependency eqs s0 (Mul x y)   = dependency eqs s0 x ++ dependency eqs s0 y
dependency eqs s0 (Div x y)   = dependency eqs s0 x ++ dependency eqs s0 y
dependency eqs s0 (Norm x)    = dependency eqs s0 x
dependency eqs s0 (Sigma exp) = dependency eqs s0 exp

dependencyOfIntegralOperand :: Equations -> Exp -> [String]
dependencyOfIntegralOperand eqs (Integral _)
  = error "dependencyOfIntegralOperand: integral in another integral"
dependencyOfIntegralOperand eqs (Constant _)
  = []
dependencyOfIntegralOperand eqs (Var symbol)
  = let exp = equationExp $ equation eqs symbol
    in case exp of
         (Integral exp') -> dependencyOfIntegralOperand eqs exp' ++ [symbol]
         otherwise       -> []
dependencyOfIntegralOperand eqs (Var' symbol)
  = let exp = equationExp $ equation eqs symbol
    in case exp of
         (Integral exp') -> dependencyOfIntegralOperand eqs exp' ++ [symbol]
         otherwise       -> []
dependencyOfIntegralOperand eqs (Add x y)
  = dependencyOfIntegralOperand eqs x ++ dependencyOfIntegralOperand eqs y
dependencyOfIntegralOperand eqs (Sub x y)
  = dependencyOfIntegralOperand eqs x ++ dependencyOfIntegralOperand eqs y
dependencyOfIntegralOperand eqs (Mul x y)
  = dependencyOfIntegralOperand eqs x ++ dependencyOfIntegralOperand eqs y
dependencyOfIntegralOperand eqs (Div x y)
  = dependencyOfIntegralOperand eqs x ++ dependencyOfIntegralOperand eqs y
dependencyOfIntegralOperand eqs (Norm x)
  = dependencyOfIntegralOperand eqs x
dependencyOfIntegralOperand eqs (Sigma _)
  = error "dependencyOfIntegralOperand: sigma in integral"

-- todo: should be rewrite
checkEquations :: Equations -> Equations
checkEquations eqs
  = if all aux (equations eqs)
    then if all aux2 (equations eqs)
         then if all aux3 (equations eqs)
              then eqs
              else undefined
         else undefined
    else error "checkEquations: invalid equations"
  where aux (symbol,eq)  = checkEquation eqs eq
        aux2(symbol,eq)  = checkEquationWithInitialCondition eqs eq
        aux3 (symbol,eq) = checkEquationTypeWithInitialCondition eqs eq

checkEquationWithInitialCondition :: Equations -> Equation -> Bool
checkEquationWithInitialCondition eqs eq
  | isIntegral (equationExp eq) = True
  | otherwise
      = case equationsInitialCondition eqs (equationSymbol eq) of
          Just x  -> error msg
          Nothing -> True
  where msg = "checkEquations: non-integral equation with initial condition"
              ++ ", which would be unused"

checkEquationTypeWithInitialCondition :: Equations -> Equation -> Bool
checkEquationTypeWithInitialCondition eqs eq
  | isIntegral (equationExp eq)
      = case equationsInitialCondition eqs (equationSymbol eq) of
          Just x  -> if equationType eq == initialConditionType x
                     then True
                     else error "checkEquation: type confliction"
          Nothing -> True
  | otherwise = True


-- Definition of Equation

data Equation = Equation { equationSymbol :: String
                         , equationType   :: ExpType
                         , equationExp    :: Exp
                         }

instance Show Equation where
  show eq = (equationSymbol eq) ++ " = " ++ show (equationExp eq) ++ " :: " ++ show (equationType eq)

makeEquation = Equation

checkEquation :: Equations -> Equation -> Bool
checkEquation eqs eq = checkExp eqs exp
  where exp = equationExp eq


-- Definition of Exp

data Exp = Integral Exp
         | Constant ExpValue
         | Var String
         | Var' String
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Norm Exp
         | Sigma Exp

instance Show Exp where
  show (Constant x)   = show x
  show (Integral exp) = showIntegral exp
  show (Var symbol)   = symbol
  show (Var' symbol)  = symbol ++ "'"
  show (Add x y)      = showBinaryOp "+" x y
  show (Sub x y)      = showBinaryOp "-" x y
  show (Mul x y)      = showBinaryOp "*" x y
  show (Div x y)      = showBinaryOp "/" x y
  show (Norm x)       = showNorm x
  show (Sigma exp)    = showSigma exp

showIntegral :: Exp -> String
showIntegral exp = "integral " ++ showWithParenthesisIfNotAtom exp

showSigma :: Exp -> String
showSigma exp = "Σ" ++ showWithParenthesisIfNotAtom exp

showBinaryOp :: String -> Exp -> Exp -> String
showBinaryOp op x y = x' ++ " " ++ op ++ " " ++ y'
  where x' = showWithParenthesisIfNotAtom x
        y' = showWithParenthesisIfNotAtom y

showNorm :: Exp -> String
showNorm x = "|" ++ show x ++ "|"

showWithParenthesisIfNotAtom :: Exp -> String
showWithParenthesisIfNotAtom exp
  | isAtom exp = show exp
  | otherwise  = parenthesis $ show exp

parenthesis :: String -> String
parenthesis x = "(" ++ x ++ ")"

isAtom :: Exp -> Bool
isAtom (Constant _) = True
isAtom (Var _)      = True
isAtom (Var' _)     = True
isAtom (Norm _)     = True
isAtom _            = False

integral :: Exp -> Exp
integral = Integral

constantS :: Scalar -> Exp
constantS = Constant . ExpValueScalar

constantV :: Vector3 -> Exp
constantV = Constant . ExpValueVector

var, var' :: String -> Exp
var  = Var
var' = Var'

norm :: Exp -> Exp
norm = Norm

sigma :: Exp -> Exp
sigma = Sigma

(<+>), (<->), (<*>), (</>) :: Exp -> Exp -> Exp
(<+>) = Add
(<->) = Sub
(<*>) = Mul
(</>) = Div

checkExp :: Equations -> Exp -> Bool
checkExp eqs (Integral exp) = not (containIntegral exp) &&
                              not (containSigma exp) &&
                              checkExp eqs exp
checkExp _   (Constant _)   = True
checkExp _   (Var _)        = True
checkExp _   (Var' _)       = True
checkExp eqs (Add x y)      = checkExpBinOp eqs x y
checkExp eqs (Sub x y)      = checkExpBinOp eqs x y
checkExp eqs (Mul x y)      = checkExpBinOp eqs x y
checkExp eqs (Div x y)      = checkExpBinOp eqs x y
checkExp eqs (Norm x)       = checkExpUnaryOp eqs x
checkExp eqs (Sigma exp)    = not (containIntegral exp) &&
                              not (containSigma exp) &&
                              checkExp eqs exp

checkExpUnaryOp :: Equations -> Exp -> Bool
checkExpUnaryOp eqs x = not (isIntegral x) && checkExp eqs x

checkExpBinOp :: Equations -> Exp -> Exp -> Bool
checkExpBinOp eqs x y = not (isIntegral x) &&
                        not (isIntegral y) &&
                        checkExp eqs x     &&
                        checkExp eqs y

containIntegral :: Exp -> Bool
containIntegral (Add x y)    = containIntegral x || containIntegral y
containIntegral (Sub x y)    = containIntegral x || containIntegral y
containIntegral (Mul x y)    = containIntegral x || containIntegral y
containIntegral (Div x y)    = containIntegral x || containIntegral y
containIntegral (Norm x)     = containIntegral x
containIntegral exp          = isIntegral exp

isIntegral :: Exp -> Bool
isIntegral (Integral _) = True
isIntegral _            = False

containSigma :: Exp -> Bool
containSigma (Add x y) = containSigma x || containSigma y
containSigma (Sub x y) = containSigma x || containSigma y
containSigma (Mul x y) = containSigma x || containSigma y
containSigma (Div x y) = containSigma x || containSigma y
containSigma (Norm x)  = containSigma x
containSigma exp       = isSigma exp

isSigma :: Exp -> Bool
isSigma (Sigma _) = True
isSigma _         = False


-- Definition of ExpValue

data ExpValue = ExpValueScalar Scalar
              | ExpValueVector Vector3
  deriving Eq

instance Show ExpValue where
  show (ExpValueScalar x) = show x
  show (ExpValueVector x) = show x


-- Definition of ExpType

data ExpType = ExpTypeScalar
             | ExpTypeVector
  deriving Eq

instance Show ExpType where
  show ExpTypeScalar = "Scalar"
  show ExpTypeVector = "Vector"


-- Definition of InitialCondition

data InitialCondition = InitialConditionScalar (Array Scalar)
                      | InitialConditionVector (Array Vector3)

instance Show InitialCondition where
  show _ = "..."

initialConditionArraySize :: InitialCondition -> Int
initialConditionArraySize (InitialConditionScalar x) = V.length x
initialConditionArraySize (InitialConditionVector x) = V.length x

initialConditionType :: InitialCondition -> ExpType
initialConditionType (InitialConditionScalar _) = ExpTypeScalar
initialConditionType (InitialConditionVector _) = ExpTypeVector


-- examples

nBody =
  do define         "x" (integral (var "v" <*> dt))
     define         "v" (integral (var "a" <*> dt))
     defineWithType "a" (var "f" </> m) ExpTypeVector
     define         "f" (let r = norm (var' "x" <-> var "x")
                             k = m <*> m <*> g </> r </> r
                             n = (var' "x" <-> var "x") </> r
                         in sigma (k <*> n))
     initialConditionV "x" x0
  where dt = constantS 0.01
        m  = constantS 1
        g  = constantS 9.8
        x0 =  V.fromList [ (0,0,0), (2,0,0), (0,2,0) ]
--        x0 =  V.fromList [ (i,i,i)
--                         | i <- [0..99]]

main = mapM_ (printRow . flip machineRegisterValueV "x")
     $ take 100 $ runSimMachine nBody

printRow :: Array Vector3 -> IO ()
printRow = putStrLn . rowString . V.toList

rowString :: [Vector3] -> String
rowString = join "," . map vec3String

vec3String :: Vector3 -> String
vec3String (x,y,z) = join "," [ show x, show y, show z ]

join :: String -> [String] -> String
join _   []         = ""
join sep (x:[])     = x
join sep (x:xs) = x ++ sep ++ join sep xs
