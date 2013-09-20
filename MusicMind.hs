
-- @author: Albert Wang
-- @ID    : 331793

module MusicMind 
(initialGuess
,nextGuess
,GameState
,testMethod
) where

-- |Debugging packages
import Debug.Hood.Observe
import Debug.Trace
import Data.List
	
-- |fst GameState is a list of all previous guesses.snd GameState is the result of previous guess.	
type GameState = ( [[String]] , (Int,Int,Int)) 	


-- |Initial guess is randomly chosen.	
-- |GameState (0,0,0) means the first guess.

initialGuess :: ([String],GameState)
initialGuess = (["A1","B1","C2"],(["A1","B1","C2"]:[],(0,0,0)))

-- |Debug code.
nextGuess = observe "Arguments for nextGuess' " nextGuess'

nextGuess' :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
-- |Debug code. Trace the input value of arguments
nextGuess' record result | trace ("-- Debug: nextGuess' " ++ show record ++ " " ++ show result) False = undefined
-- |FIX ME: Change the newGuess object.
nextGuess' record result = (newGuess,(newGuess:(fst.snd) record, result) ) 
           where newGuess = ["A1","B1","C3"]

-- |Helper functions. |Private

-- |Generate all the possible combinations based on the result and gameState. 
-- Previous used combinations will not be included in this list.

-- possibleTargetsUnderResult:: (Int,Int,Int) -> GameState -> [[String]]
-- possibleTargetsUnderResult result gameState =   

allCombinations:: [String] -> (Int,Int,Int) -> [[String]]
allCombinations strList intTuple | trace ("-- Debug: allCominations " ++ show strList ++ " " ++ show intTuple) False = undefined
allCombinations strList intTuple = [ xs | xs<-combinations, ys<-combinations,(all ( `elem` ys) xs ) == False ]
         where intList = (first intTuple):(second intTuple):(third intTuple):[]
               first (x,_,_) = x
               second(_,y,_) = y
               third (_,_,z) = z               
               tempList = [ (x ++ (show n))| x <- strList,n <- intList]
               combinations =  [a:b:c:[] | a<-tempList,b<-tempList,c<-tempList, a/=b,b/=c, a/=c ]


testMethod::IO ()
testMethod = putStrLn $ (fst initialGuess) !! 0 ++ (fst initialGuess) !! 1 ++ (fst initialGuess) !! 2 



--nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)	


