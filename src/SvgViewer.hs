{-# LANGUAGE ImplicitParams, OverloadedStrings, ScopedTypeVariables #-}
import System.Environment (getArgs)

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Graphics.UI.FLTK.Theme.Light

import Control.Concurrent
import Control.Monad
import System.IO
import System.Posix.IO
import Data.IORef
import Data.Time
import Data.Either

import Reanimate.Arrow
import Reanimate.Examples
import Lucid.Svg(prettyText)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Lazy as T

animation :: Ani ()
animation = highlight

fps :: Double
fps = 1.0 / 60.0

animate :: UTCTime -> Ref Box -> IO ()
animate startingTime b = do
  now <- getCurrentTime
  let time = realToFrac (diffUTCTime now startingTime)
      svg = T.unpack (prettyText (frameAt time animation))
  iE <- svgImageNew (BC.pack svg)
  case iE of
    Left _ -> error ("The generated SVG is invalid: \n" ++ svg)
    Right i -> do
      rB <- getRectangle b
      getImage b >>= maybe (return ()) (\i -> destroy i)
      setImage b (Just i)
      redraw b
  FL.repeatTimeout fps (animate startingTime b)

ui :: (?assets :: Assets) => IO ()
ui = do
 window <- doubleWindowNew
           (Size (Width 600) (Height 400))
           Nothing
           (Just "Reanimate Viewer")
 begin window
 b <- boxNew (toRectangle (10,10,580,350)) Nothing
 setBox b FlatBox
 end window
 showWidget window
 now <- getCurrentTime
 FL.addTimeout fps (animate now b)

replMain :: IO ()
replMain = do
  assets <- configureTheme
  let ?assets = assets
  ui
  FL.replRun

main :: IO ()
main = do
  assets <- configureTheme
  let ?assets = assets
  ui
  FL.run
  FL.flush
