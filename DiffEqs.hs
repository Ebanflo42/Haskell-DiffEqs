module DiffEqs where

import Data.List

function :: (Floating a, Ord a) => a -> a -> a
function x y = x*y

euler :: (Floating a, Ord a) => a -> (a -> a -> a) -> (a, a) -> (a, a)
euler step f (x, y) = (x + step, y + step*(f x y))

loopEuler :: (Floating a, Ord a) => a -> (a -> a -> a) -> (a, a) -> a -> [(a, a)]
loopEuler step f init finX =
  if fst init >= finX then []
  else next:(loopEuler step f next finX)
  where next = euler step f init

rungeKutta :: (Floating a, Ord a) => a -> (a -> a -> a) -> (a, a) -> (a, a)
rungeKutta step f (x, y) =
  (x + step, y + step*(f x' y'))
  where x' = x + 0.5*step
        y' = y + 0.5*step*(f x y)

loopRungeKutta :: (Floating a, Ord a) => a -> (a -> a -> a) -> (a, a) -> a -> [(a, a)]
loopRungeKutta step f init finX =
    if fst init >= finX then []
    else next:(loopRungeKutta step f next finX)
    where next = rungeKutta step f init

doThing :: String -> IO ()
doThing str =
  case read str :: Int of
    1 -> do
      putStrLn "Initial condition? Give your response in the form of an ordered pair."
      init <- getLine
      putStrLn "Step size?"
      step <- getLine
      putStrLn "Final x value?"
      x <- getLine
      putStrLn $ intercalate "\n" $ map show $
        loopEuler (read step :: Double) function (read init :: (Double, Double)) (read x :: Double)
      main
    2 -> do
      putStrLn "Initial condition? Give your response in the form of an ordered pair."
      init <- getLine
      putStrLn "Step size?"
      step <- getLine
      putStrLn "Final x value?"
      x <- getLine
      putStrLn $ intercalate "\n" $ map show $
        loopRungeKutta (read step :: Double) function (read init :: (Double, Double)) (read x :: Double)
      main
    _ ->
      putStrLn "Please give valid input."
        >> main

main :: IO ()
main =
  putStrLn "1 for first-order Runge-Kutta (Euler's method), 2 for second-order."
    >> getLine >>= doThing