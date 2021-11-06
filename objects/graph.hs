
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}


module Objects.Graph where

import Util
import Data.List hiding (delete, intersect) 
import Data.Maybe 

class Graph g where
  delete   :: Eq v => v -> g v -> g v
  link     :: Eq v => v -> v -> g v -> g v
  unlink   :: Eq v => v -> v -> g v -> g v
  neighborhood :: Eq v => g v -> v -> [v]

  {-# MINIMAL delete, link, unlink, neighborhood #-} 

  adjacent :: Eq v => g v -> v -> v -> Bool
  adjacent g v u = elem u $ neighborhood g v  

  loop :: Eq v => v -> g v -> g v
  loop v = link v v

  unloop :: Eq v => v -> g v -> g v
  unloop v = unlink v v 

  clear :: Eq v => v -> g v -> g v
  clear v g = foldr (unlink v) g $ neighborhood g v

  contract :: Eq v => v -> v -> g v -> g v
  contract v u g = foldr (link v) (delete u g) $ neighborhood g u

  quotient :: Eq v => [v] -> g v -> g v
  quotient []  = id
  quotient [v] = id
  quotient (v:u:vs) = quotient (v:vs) . contract v u

  degree :: Eq v => g v -> v -> Int
  degree g = length . (neighborhood g)

  distance :: (Ord a, Fractional a, Eq v) => g v -> v -> v -> a
  distance g v u 
    | null $ ds = 1 / 0
    | otherwise = snd $ head ds
    where ds = dijkstras u g [(v, 0)]

  dijkstras :: (Ord a, Fractional a, Eq v) => v -> g v -> [(v, a)] -> [(v, a)]
  dijkstras u g ds
    | null ds = ds  
    | v == u = ds
    | otherwise = dijkstras u (clear v g) $ tail ds ++ zip (neighborhood g v) (repeat $ d + 1)
    where (v, d) = head ds

  clique   :: (Eq v) => g v -> [v] -> Bool
  clique   g vs = all id  [adjacent g v u | v <- vs, u <- vs, v /= u]

  coclique :: (Eq v) => g v -> [v] -> Bool
  coclique g vs = all not [adjacent g v u | v <- vs, u <- vs, v /= u]

  component :: (Eq v) => g v -> v -> [v]
  component g v
    | null $ neighborhood g v = [v]
    | otherwise = (:) v $ foldr union [] $ map (component $ delete v g) $ neighborhood g v


class Graph g => FiniteGraph g where
  vertices :: Eq v => g v -> [v]
  complete :: Eq v => [v] -> g v

  {-# MINIMAL vertices, complete #-}

  complement :: Eq v => g v -> g v
  complement g = foldr (uncurry unlink) (complete $ vertices g) [(v, u) | v <- vertices g, u <- neighborhood g v]

  empty :: Eq v => [v] -> g v
  empty = complement.complete

  path :: Eq v => [v] -> g v
  path [v] = empty [v]
  path (v:u:vs) = complete [v,u] `join` path (u:vs)

  cycle :: Eq v => [v] -> g v
  cycle vs = link (head vs) (last vs) $ path vs

  join :: Eq v => g v -> g v -> g v
  join g1 g2 = foldr ($) (empty vs) $ map (uncurry link) [(v, u) | v <- vs, u <- vs, adjacent g1 v u || adjacent g2 v u]
    where vs = union (vertices g1) $ vertices g2

  intersect :: Eq v => g v -> g v -> g v
  intersect g1 g2 = foldr ($) (empty vs) $ map (uncurry link) [(v, u) | v <- vs, u <- vs, adjacent g1 v u && adjacent g2 v u]
    where vs = union (vertices g1) $ vertices g2

  fromGraph :: (Eq v, FiniteGraph h) => h v -> g v
  fromGraph g = foldr (uncurry link) (empty $ vertices g) [(v, u) | v <- vertices g, u <- neighborhood g v]

  subgraph :: Eq v => g v -> [v] -> g v
  subgraph g vs = foldr delete g $ vertices g \\ vs

  order :: Eq v => g v -> Int
  order = length . vertices

  size :: Eq v => g v -> Int
  size g = sum $ map (degree g) $ vertices g

  girth :: (Ord a, Fractional a, Eq v) => g v -> a
  girth g
    | null vs = 1/0 
    | otherwise = min (1 + distance g' v u) $ girth g'
    where vs = filter ((>0).degree g) $ vertices g; v = head vs; u = head $ neighborhood g v; g' = unlink v u g

  diameter :: (Ord a, Fractional a, Eq v) => g v -> a
  diameter g = maximum [distance g v u | v <- vertices g, u <- vertices g]

  maximumDegree :: (Eq v) => g v -> Int
  maximumDegree g = maximum [degree g v | v <- vertices g]

  minimumDegree :: (Eq v) => g v -> Int
  minimumDegree g = minimum [degree g v | v <- vertices g]

  cliqueNumber :: (Eq v) => g v -> Int
  cliqueNumber g 
    | clique g (vertices g) = order g
    | otherwise = 1 + maximum [cliqueNumber $ subgraph g $ neighborhood g v | v <- vertices g]

  cocliqueNumber :: (Eq v) => g v -> Int
  cocliqueNumber = cliqueNumber . complement

  coloringNumber :: (Eq v) => g v -> Int
  coloringNumber g
    | order g <= 1 = 1
    | otherwise = max (1 + degree g v) $ coloringNumber $ delete v g where
    v = head $ filter ((== minimumDegree g).degree g) $ vertices g

  coloring :: (Eq v, Eq a) => g v -> (v -> a) -> Bool
  coloring g c = null [v | v <- vertices g, u <- neighborhood g v, c v == c u]

  connected :: (Eq v) => g v -> Bool
  connected g = order g == length (component g $ head $ vertices g)

data RelationGraph a = RG [a] (a -> a -> Bool)
instance Graph RelationGraph where
  adjacent (RG vs f) v u = f v u
  neighborhood g v = filter (adjacent g v) $ vertices g
  delete v (RG vs f) = RG (without v vs) f
  unlink v u (RG vs f) = RG vs $ curry $ replace ((v,u), False) $ uncurry f
  link   v u (RG vs f) = RG vs $ curry $ replace ((v,u), True ) $ uncurry f
instance FiniteGraph RelationGraph where
  vertices (RG vs f) = vs
  complete vs = RG vs $ const $ const True
  empty    vs = RG vs $ const $ const False
instance (Eq a, Show a) => Show (RelationGraph a) where
  show = show.toPairGraph

data NeighborhoodGraph a = NG [a] (a -> [a])
instance Graph NeighborhoodGraph where
  neighborhood (NG vs f) v = f v
  delete v (NG vs f) = NG (without v vs) f
  unlink v u (NG vs f) = NG vs (\v' -> if
                                    | v' == v   -> without u $ f v
                                    | v' == u   -> without v $ f u
                                    | otherwise -> f v')
  link   v u (NG vs f) = NG vs (\v' -> if
                                    | v' == v   -> u : f v
                                    | v' == u   -> v : f u
                                    | otherwise -> f v')
instance FiniteGraph NeighborhoodGraph where
  vertices (NG vs f) = vs
  complete vs = NG vs $ const vs
  empty vs = NG vs $ const mempty
instance Show a => Show (NeighborhoodGraph a) where
  show (NG vs f) = show $ zip vs $ map f vs
instance Eq a => Eq (NeighborhoodGraph a) where
  NG vs f == NG us g = (null $ vs \\ us) && (all null [f v \\ g u | v <- vs, u <- us, v == u])

data PairGraph a = PG [a] [(a, a)] deriving (Read, Show, Eq)
instance Graph PairGraph where
  adjacent (PG vs es) v u = elem (v, u) $ es
  neighborhood g v = filter (adjacent g v) $ vertices g
  delete v (PG vs es) = PG (filter (/= v) vs) $ filter ((/=v).snd) $ filter ((/=v).fst) es
  unlink v u (PG vs es) = PG vs $ es \\ [(v, u), (u, v)]
  link   v u (PG vs es) = PG vs $ (v, u):(u, v):es
instance FiniteGraph PairGraph where
  vertices (PG g _)  = g
  complete vs = PG vs [(v, u) | v <- vs, u <- vs, v /= u]
  empty vs = PG vs []
  
data WordGraph a = WG [[a]] deriving (Read, Show, Eq)
instance Graph WordGraph where
  adjacent (WG ws) v u = not $ null $ filter (elem v) $ map (remove u) $ filter (elem u) ws
  neighborhood g v = filter (adjacent g v) $ vertices g
  delete v (WG ws) = WG $ map (without v) ws
  link   u v (WG ws) = WG $ [u,v] : ws
  unlink u v (WG ws) = WG [if elem v w then without u w else w | w <- ws]
instance FiniteGraph WordGraph where
  vertices (WG ws) = foldr union [] ws
  complete vs = WG $ pure $ vs
  empty    vs = WG $ map pure vs

data AdjacencyMatrix a = AM [[Bool]] [a] deriving (Read, Show, Eq)
instance Graph AdjacencyMatrix where
  adjacent (AM m vs) v u
    | isNothing i || isNothing j = False
    | otherwise = m !!fromJust i!!fromJust j where
    i = findIndex (==v) vs
    j = findIndex (==u) vs
  neighborhood g v = filter (adjacent g v) $ vertices g
  delete v (AM m vs) = AM (map f $ f m) $ without v vs where
    f = map snd . filter ((== v) . fst) . zip vs 
  link   v u (AM m vs) = AM (zipWith (zipWith (||)) m [[      w == v && z == u || w == u && z == v | z <- vs] | w <- vs]) vs
  unlink v u (AM m vs) = AM (zipWith (zipWith (&&)) m [[not $ w == v && z == u || w == u && z == v | z <- vs] | w <- vs]) vs
instance FiniteGraph AdjacencyMatrix where
  vertices (AM m vs) = vs
  complete vs = AM [[v /= u | u <- vs] | v <- vs] vs where n = length vs
  empty vs = AM (take n $ repeat $ take n $ repeat False) vs where n = length vs

data IncidenceMatrix a = IM [[Bool]] [a] deriving (Read, Show, Eq)
instance Graph IncidenceMatrix where
  adjacent (IM m vs) v u 
    | isNothing i || isNothing j = False
    | otherwise = any id [s!!fromJust i && s!!fromJust j | s <- m] where
    i = findIndex (==v) vs
    j = findIndex (==u) vs
  neighborhood g v = filter (adjacent g v) $ vertices g
  delete v (IM m vs) = IM (map f m) $ without v vs where
    f = map snd . filter ((== v) . fst) . zip vs
  link v u (IM m vs) = flip IM vs $ map (flip elem [v, u]) vs : m
  unlink v u (IM m vs)
    | isNothing i || isNothing j = IM m vs
    | otherwise = flip IM vs $ filter (\xs -> not $ xs!!fromJust i && xs!!fromJust j) m where
    i = findIndex (==v) vs
    j = findIndex (==u) vs
instance FiniteGraph IncidenceMatrix where
  vertices (IM m vs) = vs
  complete vs = flip IM vs [zipWith (||) (map (==v) vs) (map (==u) vs) | v <- vs, u <- vs]
  empty vs = IM [] vs

toPairGraph :: (FiniteGraph g, Eq v) => g v -> PairGraph v
toPairGraph = fromGraph
toWordGraph :: (FiniteGraph g, Eq v) => g v -> WordGraph v
toWordGraph = fromGraph

latice :: (Eq v, Num v, Integral a) => a -> v -> [v] -> NeighborhoodGraph v
latice d n vs = NG vs $ (\v -> [v' | i <- [0..d], s <- [-1, 1], let v' = v + s *  n^i, v' `elem` vs])
