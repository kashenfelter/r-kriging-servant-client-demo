{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveFunctor         #-}
module Demo where
import FreeSketch 
import           Yesod hiding (get)
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (join)
import Control.Monad.Trans.State (StateT, get, evalStateT)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Writer (WriterT, tell, runWriterT)
import Data.Text


data Demo =
  Demo {
    redPort   :: Int
  , greenPort :: Int
  , bluePort  :: Int
  }

mkYesod "Demo" [parseRoutes|
/ HomeR GET
/picture.jpg PictureR GET
/r-kriging-servant-client-demo/picture.jpg ShimPictureR GET
|]

instance Yesod Demo

getShimPictureR = getPictureR

getPictureR :: Handler ()
getPictureR = sendFile "image/jpeg" "pictures/computerhammer.png"

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Demo+"
  addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/p5.js/0.5.11/p5.js"
  addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/p5.js/0.5.11/addons/p5.dom.js"
  addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.2.1/jquery.min.js"
  y <- getYesod
  let endpoints = Prelude.map local [redPort y, greenPort y, bluePort y]
  sketchInSpace "krigingDemo" $ demoFreeSketchP5 endpoints
  toWidget [lucius|
    body {
      margin: 0px;
    }
  |]
  where local p = pack $ "http://localhost:" ++ show p ++ "/kriging"

demoFreeSketchP5 :: [APIEndpoint] -> FreeSketch Demo ()
demoFreeSketchP5 endpoints = do
  ((p5_0_0, sem_0), (image, canvas_0_0, krigingPixels, imagePixels)) <- runWriterT originalImage
  (kriging_p5s, _) <- runReaderT (runWriterT (krigingChannels sem_0)) (krigingPixels, image)
  s <- mapM (uniformPixelSampleGenerator imagePixels) ["red", "green", "blue"] 
  let (generators, samples) = unzip s 
  (image_p5s, _) <- runReaderT (runWriterT (imageChannels sem_0 samples)) (imagePixels, image)
  keys <- attachKeys endpoints generators samples ["red", "green", "blue"] krigingPixels 
  p5Instance Nothing Nothing Nothing (Just keys) [sem_0]
  return ()
  where
    attachKeys :: [APIEndpoint] -> [Action] -> [SamplePoints] -> [Color] -> Pixels -> FreeSketch Demo Action
    attachKeys endpoints generators samples colors pixels = do
      redKrige <- doKriging (endpoints!!0) (generators!!0) (samples!!0) (colors!!0) pixels
      kr <- keyTyped 'r' redKrige
      greenKrige <- doKriging (endpoints!!1) (generators!!1) (samples!!1) (colors!!1) pixels
      kg <- keyTyped 'g' greenKrige
      blueKrige <- doKriging (endpoints!!2) (generators!!2) (samples!!2) (colors!!2) pixels
      kb <- keyTyped 'b' blueKrige
      sketchSequence [Just kr, Just kg, Just kb]

doKriging :: APIEndpoint -> Action -> SamplePoints -> Color -> Pixels -> FreeSketch Demo Action
doKriging api generate sample color pixels = do
  send <- samplePointsToJSON sample
  gensend <- sketchSequence [Just generate, Just send]
  receive <- smoothPredictionsOverPixels color pixels
  (ajax, _) <- ajaxBlockingEndlessRetry api gensend receive
  return ajax

originalImage :: WriterT (Image, Canvas, Pixels, Pixels) (FreeSketch Demo) (Action, LoadedSemaphore)
originalImage = do
  (preload, image) <- lift $ loadImage ShimPictureR
  (tile, canvas) <- lift $ tileCanvasByImage (0, 0) image 
  (storeKriging, krigingPixels) <- lift $ storePixels ["red", "green", "blue"]
  draw <- lift $ drawImage image
  (storeImage, imagePixels) <- lift $ storePixels ["red", "green", "blue"]
  setup <- lift $ sketchSequence [Just tile, Just storeKriging, Just draw, Just storeImage]
  tell (image, canvas, krigingPixels, imagePixels)
  lift $ p5Instance (Just preload) (Just setup) Nothing Nothing ["true"]

imageChannels :: LoadSemaphore -> [SamplePoints] -> WriterT [Canvas] (ReaderT (Pixels, Image) (FreeSketch Demo)) [(Action, LoadedSemaphore)]
imageChannels sem samples = do
  let rgb = Prelude.drop 1 channels
  dpnts <- lift $ lift $ mapM drawSamplePoints samples
  let rgbAndPoints = Prelude.zip rgb dpnts
  (imagePixels, image) <- lift ask
  x <- lift $ lift $ mapM (\(colors, pts) -> runReaderT (runWriterT (pixelsByColors sem colors 0 (Just pts))) (imagePixels, image)) rgbAndPoints
  let (p5s, canvases) = unzip x
  tell canvases
  return p5s 
 
pixelsByColors :: LoadSemaphore -> [Color] -> Int -> Maybe Action -> WriterT Canvas (ReaderT (Pixels, Image) (FreeSketch Demo)) (Action, LoadedSemaphore)
pixelsByColors sem colors row followDraw = do
  (pixels, image) <- lift ask
  draw' <- lift $ lift $ drawPixels colors pixels
  draw <- lift $ lift $ sketchSequence [Just draw', followDraw]
  (tile, canvas) <- lift $ lift $ tileCanvasByImage (column, row) image
  tell canvas
  lift $ lift $ p5Instance Nothing (Just tile) (Just draw) Nothing [sem] 
  where
    column = case colors of
               ["red"]   -> 1
               ["green"] -> 2
               ["blue"]  -> 3
               _         -> 0

channels :: [[Color]]
channels = [["red", "green", "blue"], ["red"], ["green"], ["blue"]]

krigingChannels :: LoadSemaphore -> WriterT [Canvas] (ReaderT (Pixels, Image) (FreeSketch Demo)) [(Action, LoadedSemaphore)]
krigingChannels sem = do
  (krigingPixels, image) <- lift ask
  x <- lift $ lift $ mapM (\colors -> runReaderT (runWriterT (pixelsByColors sem colors 1 Nothing)) (krigingPixels, image)) channels 
  let (p5s, canvases) = unzip x
  tell canvases
  return p5s

prodMain :: IO ()
prodMain = do
    runDemo =<< execParser options
  where
    runDemo :: Options -> IO ()
    runDemo (Options r g b) = warp 3000 (Demo r g b)
    options = info (optionParser <**> helper)
            ( fullDesc
           <> progDesc "p5.js demo for r-servant-kriging"
           <> header "It's a demo" ) 

data Options =
  Options {
    red   :: Int
  , green :: Int
  , blue  :: Int
  }

optionParser :: Parser Options
optionParser = Options
               <$> option auto ( long "red"
                              <> short 'r'
                              <> help "Red kriging api localhost port"
                              <> showDefault
                              <> value 3001
                              <> metavar "INT" )
               <*> option auto ( long "green"
                              <> short 'g'
                              <> help "Green kriging api localhost port"
                              <> showDefault
                              <> value 3002
                              <> metavar "INT" )
               <*> option auto ( long "blue"
                              <> short 'b'
                              <> help "Blue kriging api localhost port"
                              <> showDefault
                              <> value 3003
                              <> metavar "INT" )
