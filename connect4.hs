--Authored by Grant Slatton on 2013 October 8
--All code is released to the public domain under the terms of [http://unlicense.org]

import Data.List
import Data.Ord
import Data.Maybe

--Define how you represent your GameState
--In Connect 4, it's just a list of length width of sublists of length height,
--and an Int for whose turn it is, player 1 == 1, player 2 == -1
data GameState = GameState [[Int]] Int deriving Show
--These just for global reference
width = 7
height = 6
blankBoard = GameState (replicate width $ replicate height (0::Int)) 1

--Define what a Move is
--In Connect 4, its an Int for the column
type Move = Int

possibleMoves :: GameState -> [Move]
possibleMoves gs@(GameState state _)
    | isNothing (winner gs) = map fst $ filter (\(a, b) -> head b == 0) $ zip [0..width] state
    | otherwise = []

--Must define makeMove which takes a GameState, and applies a Move to it,
--producing the next GameState. The Move might be Nothing, so it's a Maybe Move
makeMove :: GameState -> Maybe Move -> GameState
makeMove gs@(GameState state movePlayer) Nothing = gs
makeMove gs@(GameState state movePlayer) (Just moveCol)
    | numNonZero == height = gs
    | not $ elem moveCol $ possibleMoves gs = gs
    | otherwise = GameState (pre ++ newCol ++ post) (-movePlayer)
    where
        col = state !! moveCol
        nonZeroElems = filter (/= 0) col
        numNonZero = length nonZeroElems
        pre = take moveCol state
        post = reverse $ take (width-moveCol-1) $ reverse state
        newCol = [(replicate (height-numNonZero-1) (0::Int)) ++ [movePlayer] ++ nonZeroElems]

--Must define isGameComplete which takes a GameState and returns the winning player
--or Nothing if the game is not over. I return Just (Player 0) here for a tie, but 
--some games have no ties. This is merely an implementation choice for Connect 4
winner :: GameState -> Maybe Int
winner (GameState state _) 
    | minHori <= -4 = Just (-1)
    | minVert <= -4 = Just (-1)
    | minNWSE <= -4 = Just (-1)
    | minNESW <= -4 = Just (-1)
    | maxHori >= 4 = Just 1
    | maxVert >= 4 = Just 1
    | maxNWSE >= 4 = Just 1
    | maxNESW >= 4 = Just 1
    | sum (map abs (concat state)) == width*height = Just 0
    | otherwise = Nothing
    where
        minMax xs = (minimum xs, maximum xs)
        (minHori, maxHori) = minMax $ concatMap (map sum) $ map group $ transpose state
        (minVert, maxVert) = minMax $ concatMap (map sum) $ map group state
        (minNESW, maxNESW) = minMax $ concatMap (map sum) $ map group $ (transpose . skew) state
        (minNWSE, maxNWSE) = minMax $ concatMap (map sum) $ map group $ (transpose . skew . reverse) state

--Skew = Transform, used for testing diagonal wins
--111    11100
--111 -> 01110
--111    00111
skew :: [[Int]] -> [[Int]]
skew xs = skewHelper xs 0 $ (length $ head xs)-1

skewHelper :: [[Int]] -> Int -> Int -> [[Int]]
skewHelper [] _ _ = []
skewHelper (x:xs) pre post = ((replicate pre (0::Int)) ++ x ++ (replicate post (0::Int))) : skewHelper xs (pre+1) (post-1)

--Must define eval which takes a GameState and produces an integer value of the state
--from the point of view of the first player. That is, higher values mean a better
--state from the first player's perspective, lower values are better for second player
--In this function I just evaluate based on winning or not.
eval :: GameState -> Int
eval gs@(GameState state _)
    | isNothing win = sum $ zipWith (*) bitonic (map sum state)
    | otherwise = (div (maxBound::Int) 2) * (fromJust win)
    where
        win = winner gs
        bitonicUp = [1..(div (width+1) 2)]
        bitonicDown = reverse [1..(div width 2)]
        bitonic = bitonicUp ++ bitonicDown

--(depth, alpha, beta, move, failFast)
type NegaMaxData = (Int, Int, Int, Move, Bool)

negaMax :: GameState -> Int -> Maybe Move
negaMax gs depth 
    | 0 == length pms = Nothing
    | depth <= 0 = Just (head pms)
    | isNothing $ winner gs = Just move
    | otherwise = Nothing
    where
        pms = possibleMoves gs
        bestBelow = if (depth <= 0) 
                    then (head pms) 
                    else (fromJust $ negaMax gs (depth-1))
        moves = bestBelow:(delete bestBelow pms)
--        moves = pms
        initFoldVal = (depth, div (minBound::Int) 4, div (maxBound::Int) 4, head moves, False)
        toFold = zip moves $ replicate (length moves) gs
        (_, _, _, move, _) = foldl negaUpdate initFoldVal toFold

negaUpdate :: NegaMaxData -> (Move, GameState) -> NegaMaxData
negaUpdate foldval@(_, _, _, _, True) _ = foldval
negaUpdate foldval@(depth, alpha, beta, bestMove, False) (move, gs)
    | score >= beta = (depth, alpha, beta, move, True)
    | score > alpha = (depth, score, beta, move, False)
    | otherwise = foldval
    where
        score = -(negaMaxAux (makeMove gs (Just move)) (-beta) (-alpha) (depth-1))


negaMaxAux :: GameState -> Int -> Int -> Int -> Int
negaMaxAux gs@(GameState _ player) _ _ 0 = player * (eval gs)
negaMaxAux gs alpha beta depth = val
    where
        pms = possibleMoves gs
        initFoldVal = (depth, alpha, beta, alpha, False)
        toFold = zip pms $ replicate (length pms) gs
        (_, _, _, val, _) = foldl negaAuxUpdate initFoldVal toFold

--(depth, alpha, beta, returnVal, failFast)
type NegaMaxAuxData = (Int, Int, Int, Int, Bool)
negaAuxUpdate :: NegaMaxAuxData -> (Move, GameState) -> NegaMaxAuxData
negaAuxUpdate foldval@(_, _, _, _, True) _ = foldval
negaAuxUpdate foldval@(depth, alpha, beta, returnVal, False) (move, gs)
    | score >= beta = (depth, alpha, beta, score, True)
    | score > alpha = (depth, score, beta, score, False)
    | otherwise = foldval
    where
        score = -(negaMaxAux (makeMove gs (Just move)) (-beta) (-alpha) (depth-1))

prettyPrint :: GameState -> String
prettyPrint (GameState state _) = (concat (intersperse ['\n'] (map ((intersperse ' ') . (map f)) (transpose state))))++"\n\n"
    where
        f a
            | a == 0 = '.'
            | a == 1 = 'X'
            | otherwise = 'O'

main = do
        putStrLn (prettyPrint blankBoard)
        play blankBoard
        putStrLn "Game Over"

play :: GameState -> IO ()
play gs@(GameState _ 1) = do
                            move <- getLine
                            let nextState = makeMove gs $ Just (read move::Int)
                            putStrLn $ prettyPrint nextState
                            if isNothing $ winner nextState
                            then play nextState
                            else return ()

play gs@(GameState _ (-1)) = do
                                putStrLn $ prettyPrint nextState
                                if isNothing $ winner nextState
                                then play nextState
                                else return ()
                                where
                                    move = negaMax gs 7
                                    nextState = makeMove gs move
