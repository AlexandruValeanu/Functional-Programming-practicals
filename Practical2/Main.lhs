> import Geography
> import Maze

!!! Number of '-' is 3

======================================================================

Draw a maze.

***************************************
*              Question 2             *
* Complete the definition of drawMaze *
***************************************

> drawWallNS :: Maze -> Place -> String
> drawWallNS maze place = 
>   if (hasWall maze place S) then "---"
>   else "   "

> drawRowNS :: Maze -> Place -> String
> drawRowNS maze place =
>   if fst place == fst (sizeOf maze) then "+"
>   else "+" ++ (drawWallNS maze place) ++ (drawRowNS maze (move E place))

***************************************

> drawWallWE :: Maze -> Place -> String
> drawWallWE maze place = 
>   if (hasWall maze place W) then "|"
>   else " "

> drawRowWE :: Maze -> Place -> String
> drawRowWE maze place = 
>   if fst place == fst (sizeOf maze) then "|"
>   else (drawWallWE maze place) ++ "   " ++ (drawRowWE maze (move E place))

***************************************

> composeMaze :: Maze -> Int -> String
> composeMaze maze n = 
>   if n == (snd (sizeOf maze)) then (drawRowNS maze (0, n))
>   else (composeMaze maze (n + 1)) ++ "\n"++ (drawRowWE maze (0, n)) ++ "\n" ++ (drawRowNS maze (0, n))

> drawMaze :: Maze -> IO()
> drawMaze maze = putStr((composeMaze maze 0) ++ "\n")

======================================================================

Solve the maze, giving a result of type:

> type Path = [Direction]

***************************************
*            Questions 3--4           *
*     Complete the definition of      *
*              solveMaze              *
***************************************

> addNeighbor :: Maze -> (Place, Path) -> Direction -> [Place] -> [(Place, Path)]
> addNeighbor maze currentNode d visited =
>   if (hasWall maze node d) || ((move d node) `elem` visited) then []
>   else [(move d node, path ++ [d])]
>   where node = fst currentNode
>         path = snd currentNode

> addNewNodes :: Maze -> (Place, Path) -> [Place] -> [(Place, Path)]
> addNewNodes maze crNode visited = (addNeighbor maze crNode W visited) ++ (addNeighbor maze crNode S visited) ++ (addNeighbor maze crNode N visited) ++ (addNeighbor maze crNode E visited)

> solveMazeIter :: Maze -> Place -> [(Place, Path)] -> Path
> solveMazeIter maze place queue =
>  if null queue then []
>  else if node == place then path
>  else solveMazeIter maze place ((tail queue) ++ (addNewNodes maze (node, path) [])) 
>  where node = fst (head queue)
>        path = snd (head queue)

> fastSolveMazeIter :: Maze -> Place -> [(Place, Path)] -> [Place] -> Path
> fastSolveMazeIter maze place queue visited = 
>  if null queue then []
>  else if node == place then path
>  else fastSolveMazeIter maze place ((tail queue) ++ newList) (visited ++ (map fst newList))
>  where node = fst (head queue)
>        path = snd (head queue)
>        newList = (addNewNodes maze (node, path) visited)

> solveMaze :: Maze -> Place -> Place -> Path
> solveMaze maze start target = fastSolveMazeIter maze ((fst target) - 1, (snd target) - 1) [(start, [])] [start]

> solve :: Maze -> Path
> solve maze = solveMaze maze (0, 0) (sizeOf maze)

====================================================================== 

============DRAW SOLUTION=============================================

> arrow :: Direction -> String
> arrow N = " ⇧ "
> arrow S = " ⇩ "
> arrow E = " ⇨ "
> arrow W = " ⇦ "

***************************************

> findDirection :: Place -> [(Place, Direction)] -> String
> findDirection place path
>   | null path = "   "
>   | place == plc = arrow d
>   | otherwise = findDirection place (tail path)
>   where plc = fst (head path)
>         d = snd (head path)

> drawRowWE2 :: Maze -> Place -> [(Place, Direction)] -> String
> drawRowWE2 maze place path = 
>   if fst place == fst (sizeOf maze) then "|"
>   else (drawWallWE maze place) ++ (findDirection place path) ++ (drawRowWE2 maze (move E place) path)

***************************************

> composeMaze2 :: Maze -> Int -> [(Place, Direction)] -> String
> composeMaze2 maze n path = 
>   if n == (snd (sizeOf maze)) then (drawRowNS maze (0, n))
>   else (composeMaze2 maze (n + 1) path) ++ "\n"++ (drawRowWE2 maze (0, n) path) ++ "\n" ++ (drawRowNS maze (0, n))

> getPositions :: Place -> Path -> [(Place, Direction)]
> getPositions place path
>   | null path = []
>   | otherwise = (place, d) : (getPositions (move d place) (tail path))
>   where d = head path

> drawSolution :: Maze -> IO()
> drawSolution maze
>   | null path = error "no path"
>   | otherwise = putStr((composeMaze2 maze 0 (getPositions (0, 0) path)) ++ "\n")
>   where path = solve maze

======================================================================

Some test mazes.  In both cases, the task is to find a path from the bottom
left corner to the top right.

First a small one

> smallMaze :: Maze
> smallMaze = 
>   let walls = [((0,0), N), ((2,2), E), ((2,1),E), ((1,0),E), 
>                ((1,2), E), ((1,1), N)]
>   in makeMaze (4,3) walls

Now a large one.  Define a function to produce a run of walls:

> run (x,y) n E = [((x,y+i),E) | i <- [0..n-1]]
> run (x,y) n N = [((x+i,y),N) | i <- [0..n-1]]

And here's the maze.

> largeMaze :: Maze 
> largeMaze =
>   let walls = 
>         run (0,0) 3 E ++ run (1,1) 3 E ++ [((1,3),N)] ++ run (0,4) 5 E ++
>         run (2,0) 5 E ++ [((2,4),N)] ++ run (1,5) 3 E ++
>         run (1,8) 3 N ++ run (2,6) 3 E ++
>         run (3,1) 7 E ++ run (4,0) 4 N ++ run (4,1) 5 E ++ run (5,2) 3 N ++
>         run (4,6) 2 N ++ run (5,4) 3 E ++ run (6,3) 5 N ++ run (8,0) 4 E ++
>         run (6,1) 3 N ++ run (0,9) 3 N ++ run (1,10) 3 N ++ run (0,11) 3 N ++
>         run (1,12) 6 N ++ run (3,9) 4 E ++ run (4,11) 2 N ++
>         run (5,9) 3 E ++ run (4,8) 3 E ++ run (5,7) 5 N ++ run (6,4) 9 E ++
>         run (7,5) 3 N ++ run (8,4) 4 N ++ run (8,6) 3 N ++ run (10,5) 7 E ++
>         run (9,8) 3 E ++ run (8,9) 3 E ++ run (7,8) 3 E ++ run (8,11) 3 N ++
>         run (0,13) 5 N ++ run (4,14) 2 E ++ run (0,15) 2 E ++ 
>         run (1,14) 3 N ++ run (3,15) 2 E ++ run (0,17) 2 N ++ 
>         run (1,16) 2 E ++ run (2,15) 1 N ++ run (3,16) 3 N ++
>         run (2,17) 2 E ++ run (1,18) 6 N ++ run (4,17) 3 N ++ 
>         run (6,14) 7 E ++ run (5,13) 4 E ++ run (7,12) 2 E ++
>         run (8,13) 3 N ++ run (7,14) 3 N ++ run (10,14) 2 E ++
>         run (8,15) 5 N ++ run (7,16) 5 N ++ run (9,1) 2 E ++
>         run (10,0) 12 N ++ run (21,1) 1 E ++ run (10,2) 2 E ++
>         run (11,1) 7 N ++ run (17,1) 1 E ++ run (11,3) 3 E ++
>         run (12,2) 7 N ++ run (18,2) 2 E ++ run (19,1) 2 N ++
>         run (15,3) 3 N ++ run (14,4) 3 E ++ run (13,3) 3 E ++
>         run (12,4) 3 E ++ run (12,6) 3 N ++ run (11,7) 8 E ++ 
>         run (9,12) 3 N ++ run (12,14) 1 N ++ run (12,8) 10 E ++
>         run (0,19) 6 N ++ run (1,20) 6 N ++ run (7,18) 8 E ++
>         run (8,17) 1 N ++ run (8,18) 3 E ++ run (9,17) 4 E ++ 
>         run (10,18) 2 E ++ run (11,17) 2 E ++ run (10,20) 3 N ++
>         run (11,19) 3 N ++ run (12,18) 2 N ++ run (13,17) 2 N ++
>         run (13,13) 4 E ++ run (14,12) 7 N ++ run (13,11) 2 N ++
>         run (14,10) 2 E ++ run (13,9)2 E ++ run (14,8) 3 N ++ 
>         run (13,7) 3 N ++ run (15,5) 3 E ++ run (16,6) 3 E ++
>         run (18,5) 4 N ++ run (16,4) 2 N ++ run (13,20) 2 E ++
>         run (14,18) 4 E ++ run (20,2) 3 N ++ run (19,3) 2 E ++
>         run (18,4) 2 E ++ run (23,4) 1 E ++ run (22,4) 1 N ++
>         run (21,3) 1 N ++ run (20,4) 2 E ++ run (17,6) 4 N ++ 
>         run (20,7) 2 E ++ run (21,7) 2 N ++ run (21,6) 1 E ++ 
>         run (15,9) 1 E ++ run (17,8) 2 E ++ run (18,7) 2 E ++ 
>         run (19,8) 2 E ++ run (21,9) 1 E ++ run (16,9) 6 N ++
>         run (16,10) 7 N ++ run (15,11) 2 E ++ run (17,11) 5 N ++ 
>         run (14,14) 3 E ++ run (15,15) 6 E ++ run (17,14) 4 E ++
>         run (16,18) 4 E ++ run (15,17) 1 N ++ run (17,17) 3 N ++
>         run (15,13) 7 N ++ run (21,12) 2 E ++ run (16,16) 1 N ++
>         run (16,14) 1 N ++ run (17,15) 3 N ++ run (19,14) 4 N ++
>         run (20,15) 5 E ++ run (19,16) 2 N ++ run (21,16) 5 E ++
>         run (17,19) 2 E ++ run (18,20) 2 E ++ run (19,19) 2 E ++
>         run (18,18) 2 N ++ run (20,20) 3 N
>   in makeMaze (23,22) walls

And now an impossible maze

> impossibleMaze :: Maze
> impossibleMaze =
>   let walls = [((0,1), E), ((1,0),N), ((1,2), E), ((2,1), N)]
>   in makeMaze (3,3) walls

> emptyMaze :: Maze
> emptyMaze =
>   let walls = []
>   in makeMaze (5,5) walls

> strangeMaze :: Maze
> strangeMaze = 
>   let walls = 
>         run (0,0) 25 N ++ run (1, 1) 25 N ++
>         run (0,2) 25 N ++ run (1, 3) 25 N ++ 
>         run (0,4) 25 N ++ run (1, 5) 25 N ++
>         run (0,6) 25 N ++ run (1, 7) 25 N ++
>         run (0,8) 25 N ++ run (1, 9) 25 N ++
>         run (0,10) 25 N ++ run (1, 11) 25 N ++
>         run (0,12) 25 N ++ run (1, 13) 25 N ++
>         run (0,14) 25 N ++ run (1, 15) 25 N ++
>         run (0,16) 25 N ++ run (1, 17) 25 N ++
>         run (0,18) 25 N ++ run (1, 19) 25 N
>   in makeMaze (26, 21) walls

> hardMaze :: Maze
> hardMaze =
>   let walls = 
>         run (14,12) 13 E ++ run (13, 13) 12 E ++
>         run (0, 24) 14 N ++ run (15, 24) 11 N ++
>         run (5, 11) 10 N ++ run (0, 0) 20 N ++
>         run (17, 1) 20 E ++ run (2, 5) 23 N ++
>         run (5, 12) 10 E ++ run (10, 7) 17 E ++
>         run (5, 7) 10 N ++ run (0, 5) 6 N ++
>         run (2, 4) 17 E ++ run (1, 5) 16 E ++
>         run (0, 2) 20 N ++ run (0, 3) 20 N ++
>         run (0, 4) 20 N ++ run (20, 2) 22 E ++
>         run (4, 10) 5 N ++ run (1, 21) 5 N ++
>         run (6, 23) 5 N ++ run (0, 22) 9 N ++
>         run (8, 15) 8 E
>   in makeMaze (26, 26) walls

> huuugeMaze :: Maze
> huuugeMaze = 
>   let walls = 
>         run (0,0) 100 N ++ run (1, 1) 100 N ++
>         run (0,2) 100 N ++ run (1, 3) 100 N ++ 
>         run (0,4) 100 N ++ run (1, 5) 100 N ++
>         run (0,6) 100 N ++ run (1, 7) 100 N ++
>         run (0,8) 100 N ++ run (1, 9) 100 N ++
>         run (0,10) 100 N ++ run (1, 11) 100 N ++
>         run (0,12) 100 N ++ run (1, 13) 100 N ++
>         run (0,14) 100 N ++ run (1, 15) 100 N ++
>         run (0,16) 100 N ++ run (1, 17) 100 N ++
>         run (0,18) 100 N ++ run (1, 19) 100 N ++
>         run (0,20) 100 N ++ run (1, 21) 100 N ++
>         run (0,22) 100 N ++ run (1, 23) 100 N ++
>         run (0,24) 100 N ++ run (1, 25) 100 N ++
>         run (0,26) 100 N ++ run (1, 27) 100 N ++
>         run (0,28) 100 N ++ run (1, 29) 100 N ++
>         run (0,30) 100 N ++ run (1, 31) 100 N ++
>         run (0,32) 100 N ++ run (1, 33) 100 N ++
>         run (0,34) 100 N ++ run (1, 35) 100 N ++
>         run (0,36) 100 N ++ run (1, 37) 100 N ++
>         run (0,38) 100 N ++ run (1, 39) 100 N ++
>         run (0,40) 100 N ++ run (1, 41) 100 N ++
>         run (0,42) 100 N ++ run (1, 43) 100 N ++
>         run (0,44) 100 N ++ run (1, 45) 100 N ++
>         run (0,46) 100 N ++ run (1, 47) 100 N ++
>         run (0,48) 100 N ++ run (1, 49) 100 N ++
>         run (0,50) 100 N ++ run (1, 51) 100 N ++
>         run (0,52) 100 N ++ run (1, 53) 100 N ++
>         run (0,54) 100 N ++ run (1, 55) 100 N ++
>         run (0,56) 100 N ++ run (1, 57) 100 N ++
>         run (0,58) 100 N ++ run (1, 59) 100 N ++
>         run (0,60) 100 N ++ run (1, 61) 100 N ++
>         run (0,62) 100 N ++ run (1, 63) 100 N ++
>         run (0,64) 100 N ++ run (1, 65) 100 N ++
>         run (0,66) 100 N ++ run (1, 67) 100 N ++
>         run (0,68) 100 N ++ run (1, 69) 100 N ++
>         run (0,70) 100 N ++ run (1, 71) 100 N ++
>         run (0,72) 100 N ++ run (1, 73) 100 N ++
>         run (0,74) 100 N ++ run (1, 75) 100 N
>   in makeMaze (101, 76) walls

