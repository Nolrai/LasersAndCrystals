module Main where

import GameObject
import Graphics.Gloss

main :: IO ()
main =
  displayPicture "Hello World" (800, 600) (drawGameShine . makeShine $ testWorld)

displayPicture :: String -> (Int, Int) -> Picture -> IO ()
displayPicture title (width, height) =
  display (InWindow title (width, height) (0, 0)) black
