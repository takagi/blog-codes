{- |
  This module provides several small vectors over @Double@ values.
  All fields are strict and unpacked, so using these should be
  fairly efficient. Each size of vector is a seperate type. It also
  provides a few vector constants to save you some typing now and
  then.
-}

module Data.Vector where

{- | The type of @Vector@ fields. -}
type Scalar = Double

{- |
  The @Vector@ class. All vectors are members of this class,
  and it provides ways to apply functions over vectors.
  Typically this methods aren't used directly; rather, the
  other class instances for each vector are implemented
  in terms of these.
-}
class Vector v where
  fromScalar :: Scalar -> v
  vmap       :: (Scalar -> Scalar) -> v -> v
  vzip       :: (Scalar -> Scalar -> Scalar) -> v -> v -> v
  vfold      :: (Scalar -> Scalar -> Scalar) -> v -> Scalar

{- |
  Takes the /dot product/ of two vectors [of the same dimension].
  If you remember your highschool linear algebra, the dot product
  of two vectors V and W is equal to |V| * |W| * cos k, where
  |V| is the length of vector V, and k is the minimum angle
  between the two vectors.
-}
vdot :: Vector v => v -> v -> Scalar
vdot v w = vfold (+) $ vzip (*) v w

{- |
  Returns the /magnitude/ of a vector (that is, it's length).
  Note that this is always positive or zero (never negative).
-}
vmag :: Vector v => v -> Scalar
vmag v = sqrt $ v `vdot` v

{- |
  Multiply a vector by a scalar. This scales the magnitude
  (length) of the vector, but leaves its length unchanged.
  (Except in the case of a negative scalar, in which case
  the vector's direction is reversed.)
-}
(*<>) :: Vector v => Scalar -> v -> v
s *<> v = vmap (s*) v



{- |
  The type of 2-dimensional vectors. It provides various
  class instances such as 'Eq', 'Num', 'Show', etc.
-}
data Vector2 = Vector2 {v2x, v2y :: {-# UNPACK #-} !Scalar}
  deriving (Eq, Ord, Show)

instance Vector Vector2 where
  fromScalar x = Vector2 x x
  vmap  f (Vector2 x1 y1)                 = Vector2 (f x1)    (f y1)
  vzip  f (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (f x1 x2) (f y1 y2)
  vfold f (Vector2 x1 y1)                 = f x1 y1

instance Num Vector2 where
  (+) = vzip (+)
  (-) = vzip (-)
  (*) = vzip (*)
  negate = vmap negate
  abs    = vmap abs
  signum = vmap signum
  fromInteger n = let s = fromInteger n in fromScalar s

instance Fractional Vector2 where
  (/) = vzip (/)
  recip = vmap recip
  fromRational r = let s = fromRational r in fromScalar s

-- | Constant: The unit-length X vector, (1, 0).
vector2X :: Vector2
vector2X = Vector2 1 0

-- | Constant: The unit-length Y vector, (0, 1).
vector2Y :: Vector2
vector2Y = Vector2 0 1



{- |
  The type of 3-dimensional vectors. Similar to 'Vector2'.
-}
data Vector3 = Vector3 {v3x, v3y, v3z :: {-# UNPACK #-} !Scalar}
  deriving (Eq, Ord, Show)

instance Vector Vector3 where
  fromScalar x = Vector3 x x x
  vmap  f (Vector3 x1 y1 z1)                    = Vector3 (f x1)    (f y1)    (f z1)
  vzip  f (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (f x1 x2) (f y1 y2) (f z1 z2)
  vfold f (Vector3 x1 y1 z1)                    = f x1 (f y1 z1)

instance Num Vector3 where
  (+) = vzip (+)
  (-) = vzip (-)
  (*) = vzip (*)
  negate = vmap negate
  abs    = vmap abs
  signum = vmap signum
  fromInteger n = let s = fromInteger n in fromScalar s

instance Fractional Vector3 where
  (/) = vzip (/)
  recip = vmap recip
  fromRational r = let s = fromRational r in fromScalar s

{- |
  Takes the /cross product/ of two [3D] vectors. Again, from highschool
  linear algebra, the cross product of vector V and W is a new vector
  P such that |P| = |V| * |W| * sin k (where k is the minimum angle
  between V and W), and the direction of P is perpendicular to both
  V and W. For example, @vcross 'vector3X' 'vector3Y' = 'vector3Z'@.
  Note also that @vcross w v = negate (vcross v w)@.
-}
vcross :: Vector3 -> Vector3 -> Vector3
vcross (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3
  {
    v3x = y1 * z2   -   y2 * z1,
    v3y = x1 * z2   -   x2 * z1,
    v3z = x1 * y2   -   x2 * y1
  }

-- | Constant: The unit-length X vector, (1, 0, 0).
vector3X :: Vector3
vector3X = Vector3 1 0 0

-- | Constant: The unit-length Y vector, (0, 1, 0).
vector3Y :: Vector3
vector3Y = Vector3 0 1 0

-- | Constant: The unit-length Z vector, (0, 0, 1).
vector3Z :: Vector3
vector3Z = Vector3 0 0 1
