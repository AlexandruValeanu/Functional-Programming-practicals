Module to define the type of a maze

> module MyMazeBST (
>   Maze, 
>   makeMaze, -- :: Size -> [Wall] -> Maze
>   hasWall,  -- :: Maze -> Place -> Direction -> Bool
>   allDepths,
>   allSizes,
>   sizeOf    -- :: Maze -> Size
> )
> where

> import Geography
> import BST
> import Data.List

We'll represent a maze by its size and 4 lists of its walls (N / S / E / W).

> data Maze = AMaze Size (BST Place) (BST Place) (BST Place) (BST Place)

> reflect :: Wall -> Wall
> reflect ((i,j), d) = (move d (i,j), opposite d)

> getWalls :: [Wall] -> Direction -> [Place]
> getWalls [] _ = []
> getWalls (w:ws) d
>   | snd w == d = (fst w) : getWalls ws d
>   | otherwise = getWalls ws d


> makeMaze :: Size -> [Wall] -> Maze
> makeMaze (x,y) walls = 
>   let boundaries = -- the four boundaries
>         [((0,j),   W) | j <- [0..y-1]] ++ -- westerly boundary
>         [((x-1,j), E) | j <- [0..y-1]] ++ -- easterly boundary
>         [((i,0),   S) | i <- [0..x-1]] ++ -- southerly boundary
>         [((i,y-1), N) | i <- [0..x-1]]    -- northerly boundary
>       allWalls = walls ++ boundaries ++ map reflect (walls ++ boundaries)
>       westWalls  = buildTree (sort (getWalls allWalls W))
>       eastWalls  = buildTree (sort (getWalls allWalls E))
>       southWalls = buildTree (sort (getWalls allWalls S))
>       northWalls = buildTree (sort (getWalls allWalls N))
>  in AMaze (x,y) northWalls southWalls eastWalls westWalls

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (AMaze _ northBST southBST eastBST westBST) pos d
>   | d == N = findKey northBST pos
>   | d == S = findKey southBST pos
>   | d == E = findKey eastBST pos
>   | d == W = findKey westBST pos
>   | otherwise = error "Error hasWall"

> allDepths :: Maze -> (Int, Int, Int, Int)
> allDepths (AMaze _ n s e w) = (depth n, depth s, depth e, depth w)

> allSizes :: Maze -> (Int, Int, Int, Int)
> allSizes (AMaze _ n s e w) = (size n, size s, size e, size w)

> sizeOf :: Maze -> Size
> sizeOf (AMaze size _ _ _ _) = size
