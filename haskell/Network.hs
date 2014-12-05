module Network where

import Data.List
import qualified Data.List.Key as K
import Data.Map ((!), fromList, fromListWith, adjust, keys, Map)

data Arc = A { tail :: Node
             , head :: Node
             , cost :: Double
             , capacity :: Double } deriving (Eq, Show)

instance Ord Arc where
    (A t1 h1 _ _) `compare` (A t2 h2 _ _) = (t1,h1) `compare` (t2,h2) 

type Node = Int
type Path = [Arc]

data Graph = E |NW { nodes :: [Node]
                   , arcs  :: [Arc] } deriving Show

newGraph :: Graph
newGraph = NW [] [] 

arcList :: Int -> Graph -> [Arc]
arcList _ E = []
arcList i (NW _ a) = filter (\(A t _ _ _) -> t == i) a

-- test graph from the book
exampleGraph = NW [1,3,4,5,7,9] 
                  [A 1 2 25 30
                  ,A 1 3 35 50
                  ,A 2 4 15 40
                  ,A 3 2 45 10
                  ,A 4 3 15 30
                  ,A 4 5 45 60
                  ,A 5 3 25 20
                  ,A 5 4 35 50]

dijkstra :: Ord a => a -> Map a [(a, Double)] -> Map a (Double, Maybe a)
dijkstra source graph = f (fromList [(v, (if v== source then 0 else 1/0, Nothing)) | v <- keys graph]) (keys graph)
    where
        f ds [] = ds
        f ds q  = f (foldr relax ds $ graph ! m) (delete m q) 
            where
                m = K.minimum (fst . (ds !)) q
                relax (e,d) = adjust (min (fst (ds ! m) + d, Just m)) e

--shortestPath :: Node -> Node -> Graph -> Path
shortestPath from to graph = convertNodeListToPath ( reverse $ f to )
    where
        f x = x: maybe [] f (snd $ dijkstra from edgeMap ! x)
        edgeMap = fromListWith (++) $ (arcs graph) >>= \(A t h c _) -> [(t,[(h,c)])]
        convertNodeListToPath (x:y:xs) = (Data.List.head $ filter (\(A t h _ _) -> x==t && y==h) (arcs graph)): convertNodeListToPath (y:xs)
        convertNodeListToPath _ = []
