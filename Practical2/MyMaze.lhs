Module to define the type of a maze

> module MyMaze (
>   Maze, 
>   makeMaze, -- :: Size -> [Wall] -> Maze
>   hasWall,  -- :: Maze -> Place -> Direction -> Bool
>   sizeOf    -- :: Maze -> Size
> )
> where

> import Geography

We'll represent a maze by its size and 4 lists of its walls (N / S / E / W).

> data Maze = AMaze Size [Place] [Place] [Place] [Place]

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
>       westWalls  = getWalls allWalls W
>       eastWalls  = getWalls allWalls E
>       southWalls = getWalls allWalls S
>       northWalls = getWalls allWalls N
>  in AMaze (x,y) northWalls southWalls eastWalls westWalls

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (AMaze _ northWalls southWalls eastWalls westWalls) pos d
>   | d == N = pos `elem` northWalls
>   | d == S = pos `elem` southWalls
>   | d == E = pos `elem` eastWalls
>   | d == W = pos `elem` westWalls
>   | otherwise = error "Error hasWall"

> sizeOf :: Maze -> Size
> sizeOf (AMaze size _ _ _ _) = size
