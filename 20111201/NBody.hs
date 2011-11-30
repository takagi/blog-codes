
import qualified Data.Vector as V
import Data.Vector ((!))


-- 

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

--

type Simulation = ( Array Vector3  -- x
                  , Array Vector3  -- v
                  )

m  = 1
dt = 0.01 :: Scalar
g  = 9.8

runSimulation :: Simulation -> [Simulation]
runSimulation = iterate update

update :: Simulation -> Simulation
update = updateX
       . updateV

updateX :: Simulation -> Simulation
updateX (x,v) = (x',v)
  where x' = V.imap (\i xi -> addVector xi (scaleVector dt (v!i))) x

updateV :: Simulation -> Simulation
updateV (x,v) = (x,v')
  where v' = V.imap (\i vi -> let ai = accel x v i
                              in addVector vi (scaleVector dt ai)) v

accel x v i = scaleVector (recip m) (force x v i)

force x v i = foldl1 (addVector) $ map aux (filter (/= i) [0..n-1])
  where n     = V.length x
        aux j = let r = normVector (subVector (x!j) (x!i))
                    n = scaleVector (recip r) (subVector (x!j) (x!i))
                in scaleVector (m * m * g / r / r) n

--

main = mapM_ (printRow . fst) $ take 100 $ runSimulation (xs0,vs0)

--xs0 = V.fromList [ (0,0,0), (2,0,0), (0,2,0) ]
xs0 = V.fromList [ (i,i,i) | i <- [0..99] ]
vs0 = V.fromList [ (0,0,0) | i <- [0..99] ]

printRow :: Array Vector3 -> IO ()
printRow = putStrLn . rowString . V.toList

rowString :: [Vector3] -> String
rowString = join "," . map vec3String

vec3String :: Vector3 -> String
vec3String (x,y,z) = join "," [ show x, show y, show z ]

join :: String -> [String] -> String
join _   []     = ""
join sep (x:[]) = x
join sep (x:xs) = x ++ sep ++ join sep xs
