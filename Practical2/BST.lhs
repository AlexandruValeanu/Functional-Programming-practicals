Binary search tree implementation

> module BST (
>   BST,
>   buildTree,
>   findKey,
>   insertKey,
>   depth,
>   size,
>   flatten
> )
> where

> data BST a = Empty | Node (BST a) a (BST a)

> findKey :: Ord a => BST a -> a -> Bool
> findKey Empty key = False
> findKey (Node leftSon nodeKey rightSon) key
>   | key == nodeKey = True
>   | key < nodeKey = findKey leftSon key
>   | otherwise = findKey rightSon key

> insertKey :: Ord a => BST a -> a -> BST a
> insertKey Empty key = Node Empty key Empty
> insertKey (Node leftSon nodeKey rightSon) key
>   | key == nodeKey = Node leftSon nodeKey rightSon
>   | key < nodeKey = Node (insertKey leftSon key) nodeKey rightSon
>   | otherwise = Node leftSon nodeKey (insertKey rightSon key)

> buildTree :: [a] -> BST a
> buildTree [] = Empty
> buildTree [x] = Node Empty x Empty
> buildTree xs = Node (buildTree leftHalf) key (buildTree rightHalf)
>    where middle = (length xs) `div` 2
>          key = xs !! middle
>          leftHalf = take middle xs
>          rightHalf = drop (middle + 1) xs

> depth :: BST a -> Int
> depth Empty = 0
> depth (Node leftSon nodeKey rightSon) = 1 + max (depth leftSon) (depth rightSon)

> size :: BST a -> Int
> size Empty = 0
> size (Node leftSon nodeKey rightSon) = 1 + (+) (size leftSon) (size rightSon)

> flatten :: Ord a => BST a -> [a]
> flatten Empty = []
> flatten (Node leftSon nodeKey rightSon) = (flatten leftSon) ++ [nodeKey] ++ (flatten rightSon)
