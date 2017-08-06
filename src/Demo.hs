{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveFunctor         #-}
module Demo where
import FreeSketch 
import Yesod hiding (get)
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (join)
import Control.Monad.Trans.State (StateT, get, evalStateT)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Writer (WriterT, tell, runWriterT)
import Data.Text
import Kriging.Client
import Data.Random.Source.DevRandom
import Data.Random.RVar
import Data.Random.Extras

data Demo =
  Demo {
    redPort   :: Int
  , greenPort :: Int
  , bluePort  :: Int
  }

mkYesod "Demo" [parseRoutes|
/ HomeR GET
/p5.js P5R GET
/red RedKrigingR POST
/r-kriging-servant-client-demo/red SubRedKrigingR POST
/green GreenKrigingR POST
/r-kriging-servant-client-demo/green SubGreenKrigingR POST
/blue BlueKrigingR POST
/r-kriging-servant-client-demo/blue SubBlueKrigingR POST
/r-kriging-servant-client-demo/p5.js SubP5R GET
/p5dom.js P5domR GET
/r-kriging-servant-client-demo/p5dom.js SubP5domR GET
/jquery.js JQueryR GET
/r-kriging-servant-client-demo/jquery.js SubJQueryR GET
/picture.jpg PictureR GET
/r-kriging-servant-client-demo/picture.jpg SubPictureR GET
|]

instance Yesod Demo

postSubRedKrigingR = postRedKrigingR
postSubGreenKrigingR = postGreenKrigingR
postSubBlueKrigingR = postBlueKrigingR

postRedKrigingR :: Handler Value
postRedKrigingR = do
  kd <- requireJsonBody
  y <- getYesod
  f <- liftIO $ runLocalKriging (redPort y) "" kd
  case f of
    Right fit -> returnJson fit
    _ -> notFound 

postGreenKrigingR :: Handler Value
postGreenKrigingR = do
  kd <- requireJsonBody
  y <- getYesod
  f <- liftIO $ runLocalKriging (greenPort y) "" kd
  case f of
    Right fit -> returnJson fit
    _ -> notFound 

postBlueKrigingR :: Handler Value
postBlueKrigingR = do
  kd <- requireJsonBody
  y <- getYesod
  f <- liftIO $ runLocalKriging (bluePort y) "" kd
  case f of
    Right fit -> returnJson fit
    _ -> notFound 


getP5R :: Handler ()
getP5R = sendFile "text/javascript" "thirdparty/p5.min.js"
getSubP5R = getP5R

getP5domR :: Handler ()
getP5domR = sendFile "text/javascript" "thirdparty/p5dom.js"
getSubP5domR = getP5domR

getJQueryR :: Handler ()
getJQueryR = sendFile "text/javascript" "thirdparty/jquery.min.js"
getSubJQueryR = getJQueryR

getPictureR :: Handler ()
getPictureR = do
  catpic <- liftIO $ runRVar (choice ["displeased-cat.jpg"
                                     ,"coughing-cat.jpg"
                                     ,"couch-cat.jpg"
                                     ,"pillow-cat.jpg"]) DevRandom :: Handler String
  sendFile "image/jpeg" $ "catpictures/" ++ catpic 

getSubPictureR = getPictureR

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Demo+"
  addScript SubP5R
  addScript SubP5domR
  addScript SubJQueryR
  y <- getYesod
  render <- getUrlRender
  let endpoints = Prelude.map render [SubRedKrigingR, SubGreenKrigingR, SubBlueKrigingR]
  sketch $ demoFreeSketchP5 endpoints
  toWidget [lucius|
    body {
      margin: 0px;
    }
  |] 

demoFreeSketchP5 :: [APIEndpoint] -> FreeSketch Demo ()
demoFreeSketchP5 endpoints = do
  (p5_0_0, (image, canvas_0_0, krigingPixels, imagePixels)) <- runWriterT originalImage
  (kriging_p5s, _) <- runReaderT (runWriterT krigingChannels) (krigingPixels, image)
  s <- mapM (uniformPixelSampleGenerator imagePixels) ["red", "green", "blue"] 
  let (generators, samples) = unzip s 
  (image_p5s, _) <- runReaderT (runWriterT (imageChannels samples)) (imagePixels, image)
  keys <- attachKeys endpoints generators samples ["red", "green", "blue"] krigingPixels 
  (p5_0_0_ident, mainLoaded) <- sketchP5 "true" (p5_0_0 { keyTyped' = pure keys })
  mapM (sketchP5 mainLoaded) image_p5s
  mapM (sketchP5 mainLoaded) kriging_p5s
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
      rg <- sketchSequence kr kg
      sketchSequence rg kb

       

doKriging :: APIEndpoint -> Action -> SamplePoints -> Color -> Pixels -> FreeSketch Demo Action
doKriging api generate sample color pixels = do
  send <- samplePointsToJSON sample
  gensend <- sketchSequence generate send
  receive <- smoothPredictionsOverPixels color pixels
  (ajax, _) <- ajaxBlockingEndlessRetry api gensend receive
  return ajax

originalImage :: WriterT (Image, Canvas, Pixels, Pixels) (FreeSketch Demo) (P5 Demo)
originalImage = do
  (preload, image) <- lift $ loadImage SubPictureR
  (tile, canvas) <- lift $ tileCanvasByImage (0, 0) image 
  (storeKriging, krigingPixels) <- lift $ storePixels ["red", "green", "blue"]
  draw <- lift $ drawImage image
  (storeImage, imagePixels) <- lift $ storePixels ["red", "green", "blue"]
  setup <- lift $ sketchSequence <$> (sketchSequence tile storeKriging) <*> (sketchSequence draw storeImage)
  tell (image, canvas, krigingPixels, imagePixels)
  return $ P5 (pure preload) setup void void

imageChannels :: [SamplePoints] -> WriterT [Canvas] (ReaderT (Pixels, Image) (FreeSketch Demo)) [P5 Demo]
imageChannels samples = do
  let rgb = Prelude.drop 1 channels
  (imagePixels, image) <- lift ask
  x <- lift $ lift $ mapM (\colors -> runReaderT (runWriterT (pixelsByColors colors 0)) (imagePixels, image)) rgb 
  let (p5s, canvases) = unzip x
  tell canvases
  lift $ lift $ sequence $ Prelude.zipWith drawSamples p5s samples
  where
    drawSamples :: P5 Demo -> SamplePoints -> FreeSketch Demo (P5 Demo)
    drawSamples p5 sp = do
      d <- draw p5
      s <- drawSamplePoints sp
      return $ p5 { draw = sketchSequence d s }

pixelsByColors :: [Color] -> Int -> WriterT Canvas (ReaderT (Pixels, Image) (FreeSketch Demo)) (P5 Demo)
pixelsByColors colors row = do
  (pixels, image) <- lift ask
  let draw = drawPixels colors pixels
  (tile, canvas) <- lift $ lift $ tileCanvasByImage (column, row) image
  tell canvas
  return $ P5 void (pure tile) draw void
  where
    column = case colors of
               ["red"]   -> 1
               ["green"] -> 2
               ["blue"]  -> 3
               _         -> 0

channels :: [[Color]]
channels = [["red", "green", "blue"], ["red"], ["green"], ["blue"]]

krigingChannels :: WriterT [Canvas] (ReaderT (Pixels, Image) (FreeSketch Demo)) [P5 Demo]
krigingChannels = do
  (krigingPixels, image) <- lift ask
  x <- lift $ lift $ mapM (\colors -> runReaderT (runWriterT (pixelsByColors colors 1)) (krigingPixels, image)) channels 
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
