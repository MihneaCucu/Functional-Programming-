data Tree = Empty  -- arbore vid
   | Node Int Tree Tree Tree -- arbore cu valoare de tip Int in radacina si 3 fii
      
-- extree :: Tree
-- extree = Node 4 (Node 5 Empty Empty Empty) 
--                 (Node 3 Empty Empty (Node 1 Empty Empty Empty)) Empty

class ArbInfo t where
  level :: t -> Int -- intoarce inaltimea arborelui; pt un arbore vid
                      -- se considera ca are inaltimea 0
  sumval :: t -> Int -- intoarce suma valorilor din arbore
  nrFrunze :: t -> Int -- intoarce nr de frunze al arborelui

instance ArbInfo Tree where
  level Empty = 0
  level (Node _ left middle right) = 1 + maximum [level left, level middle, level right]
  
  sumval Empty = 0
  sumval (Node value left middle right) = value + sumval left + sumval middle + sumval right
  
  nrFrunze Empty = 0
  nrFrunze (Node _ Empty Empty Empty) = 1
  nrFrunze (Node _ left middle right) = nrFrunze left + nrFrunze middle + nrFrunze right


extree :: Tree
extree = Node 4 
            (Node 5 Empty Empty Empty) 
            (Node 3 Empty Empty (Node 1 Empty Empty Empty)) 
            Empty

-- level extree
-- 3
-- sumval extree
-- 13
-- nrFrunze extree
-- 2


class Scalar a where
  zero :: a
  one :: a
  adds :: a -> a -> a
  mult :: a -> a -> a
  negates :: a -> a
  recips :: a -> a

instance Scalar Int where
  zero = 0
  one = 1
  adds = (+)
  mult = (*)
  negates = negate
  recips x = if x == 1 then 1 else error "Eroare"

instance Scalar Double where
  zero = 0.0
  one = 1.0
  adds = (+)
  mult = (*)
  negates = negate
  recips x = 1.0 / x

class (Scalar a) => Vector v a where
  zerov :: v a
  onev :: v a
  addv :: v a -> v a -> v a -- adunare vector
  smult :: a -> v a -> v a  -- inmultire cu scalare
  negatev :: v a -> v a -- negare vector


data Vector2D a = Vector2D a a deriving (Show)

instance (Scalar a) => Vector Vector2D a where
  zerov = Vector2D zero zero
  onev = Vector2D one one
  addv (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (adds x1 x2) (adds y1 y2)
  smult k (Vector2D x y) = Vector2D (mult k x) (mult k y)
  negatev (Vector2D x y) = Vector2D (negates x) (negates y)

data Vector3D a = Vector3D a a a deriving (Show)

instance (Scalar a) => Vector Vector3D a where
  zerov = Vector3D zero zero zero
  onev = Vector3D one one one
  addv (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = Vector3D (adds x1 x2) (adds y1 y2) (adds z1 z2)
  smult k (Vector3D x y z) = Vector3D (mult k x) (mult k y) (mult k z)
  negatev (Vector3D x y z) = Vector3D (negates x) (negates y) (negates z)

