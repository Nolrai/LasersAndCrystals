{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -fprof-auto-calls #-}

module Engine where
import Control.Lens
import Utils
import Relude
import Graphics.Gloss
import Graphics.Gloss.Data.Point.Arithmetic qualified as G
import Graphics.Gloss.Geometry.Line (intersectSegSeg)
import GameObject
import Orientation
import Text.Printf (printf)
import Data.List (minimumBy)
import Data.HashSet (singleton)

type GameWorld = HashSet GameObject

data GameShine = GameShine
  { _gameWorld :: !GameWorld,
    _shine :: !(HashSet Path)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

makeLenses ''GameShine

testWorld :: GameWorld
testWorld = fromList $ GameObject (0, 0) East Source :
  [GameObject (500 G.* rotatePiOver2 (toSquarePoint ori)) ori Mirror | ori <- [East ..]]

drawGameWorld :: GameWorld -> Picture
drawGameWorld = foldMap drawGameObject

makeShine :: GameWorld -> GameShine
makeShine gw = GameShine gw shine
  where
    shine = foldMap (makeShine' gw) gw

makeShine' :: GameWorld -> GameObject -> HashSet Path
makeShine' gw go =
  case go ^. shape of
    Source -> singleton (startRay gw (go ^. position) (go ^. orientation))
    Mirror -> mempty

startRay :: GameWorld -> Point -> Orientation -> Path
startRay gw pos ori = 
  [pos, colisionPoint]
  where
    colisionPoint =
      let rawColisions :: [Point]
          rawColisions = colisions gw pos endPoint
          filteredColisions :: [Point]
          filteredColisions = filter ((LT ==) . inOriOf ori pos) rawColisions
      in minimumBy (inOriOf ori) (endPoint : filteredColisions)
    endPoint = pos G.+ (1000 G.* toSquarePoint ori)

colisions :: GameWorld -> Point -> Point -> [Point]
colisions gw start end = do
  go <- toList gw
  (wallStart, wallEnd) <- toSeg go
  let Just p = intersectSegSeg start end wallStart wallEnd
  pure p

toSeg :: GameObject -> [(Point, Point)]
toSeg go = case go ^. shape of
  Source -> []
  Mirror -> [((0, -100), (100, 200))]
  -- Mirror -> [(go ^. position G.- toSquarePoint (go ^. orientation), go ^. position G.+ toSquarePoint (go ^. orientation))]

drawGameShine :: GameShine -> Picture
drawGameShine gs = drawGameWorld (gs ^. gameWorld) <> drawShine (gs ^. shine)

drawShine :: HashSet Path -> Picture
drawShine = foldMap drawPath

drawPath :: Path -> Picture
drawPath = color yellow . line
