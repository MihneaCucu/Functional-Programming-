class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert
      :: Ord key
      => key -> value -> c key value -> c key value
  clookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  keys c = map fst (toList c)

  values :: c key value -> [value]
  values c = map snd (toList c)

  toList :: c key value -> [(key, value)]

  fromList :: Ord key => [(key,value)] -> c key value
  fromList = foldr (uncurry insert) empty

newtype PairList k v
  = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
    empty = PairList []
    singleton k v = PairList [(k, v)]
    insert k v (PairList xs) = PairList ((k, v) : filter ((/= k) . fst) xs)
    clookup k (PairList xs) = Prelude.lookup k xs
    delete k (PairList xs) = PairList (filter ((/= k) . fst) xs)
    keys (PairList xs) = map fst xs
    values (PairList xs) = map snd xs
    toList (PairList xs) = xs
    fromList = PairList

data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

instance Collection SearchTree where
    empty = Empty
    singleton k v = BNode Empty k (Just v) Empty
    insert k v Empty = singleton k v
    insert k v (BNode l k' v' r)
        | k < k' = BNode (insert k v l) k' v' r
        | k > k' = BNode l k' v' (insert k v r)
        | otherwise = BNode l k (Just v) r
    clookup _ Empty = Nothing
    clookup k (BNode l k' v' r)
        | k < k' = clookup k l
        | k > k' = clookup k r
        | otherwise = v'
    delete k Empty = Empty
    delete k (BNode l k' v' r)
        | k < k' = BNode (delete k l) k' v' r
        | k > k' = BNode l k' v' (delete k r)
        | otherwise = BNode l k Nothing r
    keys = map fst . toList
    values = map snd . toList
    toList Empty = []
    toList (BNode l k v r) = toList l ++ maybeToList k v ++ toList r
      where
        maybeToList :: key -> Maybe value -> [(key, value)]
        maybeToList _ Nothing = [] 
        maybeToList k (Just v) = [(k, v)] 

data Punct = Pt [Int]

instance Show Punct where
    show (Pt coords) = showTuple coords
        where
            showTuple [] = "()"
            showTuple xs = "(" ++ init (concatMap (\x -> show x ++ ", ") xs) ++ ")"

data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArb a where
 	    toArb :: a -> Arb
	    fromArb :: Arb -> a
-- Pt [1,2,3]
-- (1, 2, 3)

-- Pt []
-- ()

-- toArb (Pt [1,2,3])
-- N (F 1) (N (F 2) (N (F 3) Vid))
-- fromArb $ N (F 1) (N (F 2) (N (F 3) Vid)) :: Punct
--  (1,2,3)

instance ToFromArb Punct where
    toArb (Pt []) = Vid
    toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))

    fromArb Vid = Pt []
    fromArb (F x) = Pt [x]
    fromArb (N left right) = 
        let (Pt l) = fromArb left
            (Pt r) = fromArb right
        in Pt (l ++ r)

data Geo a = Square a | Rectangle a a | Circle a
    deriving Show



class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

-- ghci> pi
-- 3.141592653589793

instance GeoOps Geo where
    perimeter (Square a) = 4 * a
    perimeter (Rectangle l w) = 2 * (l + w)
    perimeter (Circle r) = 2 * pi * r
    
    area (Square a) = a * a
    area (Rectangle l w) = l * w
    area (Circle r) = pi * r^2


instance (Eq a, Floating a) => Eq (Geo a) where
    g1 == g2 = perimeter g1 == perimeter g2