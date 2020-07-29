module DSW where
import Data.Tree as DTree
import Data.Tree.Pretty

data BinTree a = BinEmpty | BinNode (BinTree a) a (BinTree a)

toStringDataTree BinEmpty = Node "[]" []
toStringDataTree (BinNode xl xa xr) = Node (show xa) [toStringDataTree xl, toStringDataTree xr]

instance (Ord a, Show a, Eq a) => Show (BinTree a) where
    show BinEmpty = []
    show h = (drawVerticalTree . toStringDataTree) $ h

insert :: Ord a => a -> BinTree a -> BinTree a
insert x BinEmpty = BinNode BinEmpty x BinEmpty
insert x (BinNode l y r)
    | x <= y    = BinNode (insert x l) y r
    | otherwise = BinNode l y (insert x r)

rotateRight :: Ord a => BinTree a -> BinTree a
rotateRight (BinNode (BinNode t1 y t2) x t3) = BinNode t1 y (BinNode t2 x t3)

rotateLeft :: Ord a => BinTree a -> BinTree a
rotateLeft (BinNode t1 x (BinNode t2 y t3)) = BinNode (BinNode t1 x t2) y t3

treeToVine :: Ord a => BinTree a -> (BinTree a, Integer)
treeToVine BinEmpty = (BinEmpty, 0)
treeToVine t@(BinNode BinEmpty x r) = (BinNode BinEmpty x vine, n + 1)
    where (vine, n) = treeToVine r
treeToVine t = (treeToVine . rotateRight) t

compress :: Ord a => BinTree a -> Integer -> BinTree a
compress t 0 = t
compress t n = BinNode rl rx ct
    where
        BinNode rl rx rr = rotateLeft t
        ct = compress rr (n - 1)

vineToTree :: Ord a => (BinTree a, Integer) -> BinTree a
vineToTree (t, n) = fst $ until nLeqThanOne compressHalf (leftCompressed, n - leafCount)
    where
        nLeqThanOne = (<= 1) . snd
        compressHalf (t, n) = (compress t (n `div` 2), n `div` 2)
        leafCount = n + 1 - 2^(floor . (logBase 2.0) . fromIntegral $ (n + 1))
        leftCompressed = compress t leafCount

dsw :: Ord a => BinTree a -> BinTree a
dsw = vineToTree . treeToVine

-- Examples
m1 = BinNode (BinEmpty) 20 (BinNode (BinNode BinEmpty 6 (BinNode BinEmpty 18 BinEmpty)) 10 (BinNode BinEmpty 9 BinEmpty)) 
m2 = foldr insert BinEmpty [8,7,6,5,4,3,2,1]
