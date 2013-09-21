
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
-- type GameState = ( [[String]] , (Int,Int,Int)) 	
type GameState = [[String]]

-- |Initial guess is chosen by the developer.	

initialGuess :: ([String],GameState)
initialGuess = (["A1","B1","C2"],targetsList)
           where targetsList = allCombinations ["A","B","C","D","E","F","G"] (1,2,3)

-- |Debug code.
--nextGuess = observe "Arguments for nextGuess' " nextGuess'

nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
-- |Debug code. Trace the input value of arguments
--nextGuess record result | trace ("-- Debug: nextGuess' " ++ show record ++ " " ++ show result) False = undefined
-- |FIX ME: Change the newGuess object.
-- Result cannot be (3,0,0), since if successfully guessed the target, then this method will not be called.
nextGuess record result = (newGuess, newGameState) 
           where newGuess = head newGameState
                 newGameState = updateGameState record result

-- |Helper functions. |Private
 

-- | Get all the 1330 possible targets.
allCombinations:: [String] -> (Int,Int,Int) -> [[String]]
--allCombinations strList intTuple | trace ("-- Debug: allCominations " ++ show strList ++ " " ++ show intTuple) False = undefined
allCombinations strList intTuple = removeDuplicates combinations           
         where intList = (first intTuple):(second intTuple):(third intTuple):[]
               first (x,_,_) = x
               second(_,y,_) = y
               third (_,_,z) = z               
               tempList = [ (x ++ (show n))| x <- strList,n <- intList]
               combinations =  [a:b:c:[] | a<-tempList,b<-tempList,c<-tempList, a/=b,b/=c, a/=c ]

-- | Remove duplicate targets.For example ["A1","B2","C3"] and  ["A1","C3","B2"] 
removeDuplicates :: [[String]] ->[[String]]
-- removeDuplicates originalList | trace ("--Debug: removeDuplicates: " ++ show originalList) False = undefined
removeDuplicates [] = []
removeDuplicates originalList = if any ( `elem`  (permutations.head) originalList)  (tail originalList)   
                                  then (removeDuplicates.tail) originalList
                                  else  (head originalList):((removeDuplicates.tail) originalList)



-- | Remove those targets that are inconsistent with the result from the targets list.
-- Input GameState and result, output a new GameState.
updateGameState:: ([String],GameState) -> (Int,Int,Int) -> GameState
--updateGameState record result | trace ("-- Debug: nextGuess' " ++ show record ++ " " ++ show result) False = undefined
--updateGameState ([_,_,_],[]) (_,_,_) = [[]]
updateGameState record result = (tail.snd) record
	                            -- if checkConsistency (fst record) ((head.snd) record) result  
--                                    then ((head.snd) record):(updateGameState (fst record,(tail.snd) record) result)
-- 	                               else updateGameState (fst record,(tail.snd) record) result
	
	
checkConsistency:: [String] -> [String] -> (Int,Int,Int) -> Bool
checkConsistency lastGuess element lastResult = False
-- checkConsistency lastGuess element lastResult = if (pitch,note,oct) == lastResult
-- 	                								then True
-- 	                                                else False
-- 	           where pitch = 3 - (length  distinctElement)
-- 	                 note  = if (length distinctElement /= 0) then 1  else 0 
-- 	                 oct   = if (length distinctElement /= 0) then 1  else 0 
-- 	                 distinctElement    = element\\lastGuess
-- 	                 distinctLastResult = lastGuess\\element




 	
		
		



testMethod::IO ()
testMethod = putStrLn $ (fst initialGuess) !! 0 ++ (fst initialGuess) !! 1 ++ (fst initialGuess) !! 2 


--nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)	


