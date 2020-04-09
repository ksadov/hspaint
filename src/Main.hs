module Main where

import System.Environment
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Raster.Array
import Data.Array.Repa as R
import Data.Array.Repa.Eval as R.Eval
import Codec.Picture as JP
import Codec.Picture.Extra as JP.Extra
import Codec.Picture.Saving as JP.Save
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString.Lazy as B
import GHC.Word
import System.Exit
import qualified Config as C

data MouseState = MouseDown | MouseUp

data Brush =  Brush { sz :: Int,
                      col :: Hue,
                      dither :: (Int, Int) -> Bool}

-- |(r, g, b)
type Hue = (Int, Int, Int)

data World = World { worldMap     :: [(Int, Hue)],
                     brush        :: Brush,
                     canvasHeight :: Int,
                     canvasWidth  :: Int,
                     zoom         :: Int,
                     mouseState   :: MouseState,
                     last4marks   :: [(Int, Int)],
                     filename     :: String,
                     palette      :: VU.Vector (Int, Int, Int)
                   }

-- |[idxOfPix (x, y) w] is the [worldMap w] index corresponding to pixel (x, y)
idxOfPix :: (Int, Int) -> World -> Int
idxOfPix (x, y) w = ((canvasHeight w) - y) * (canvasWidth w) + x

-- |[idxOfPix n w] is the pixel corresponding to [worldMap w] index n
pixOfidx :: Int -> World -> (Int, Int)
pixOfidx n w = let (flipy, x) = quotRem n (canvasWidth w) in
  (x, (canvasHeight w) - flipy)

-- |[makeMark w (x, y)] is a world with [worldMap w] updated to include a
--   mark of color [col $ brush w] and width [sz $ brush w] at pixel (x, y)
makeMark ::  World -> (Int, Int) -> World
makeMark w (x, y) =
  let radius = quot (sz . brush $ w) 2 in
    let updateIdx (idx, colr) =
          let (x', y') = pixOfidx idx w in
            if (((x-x')^2 + (y-y')^2) < radius^2 && (dither $ brush w) (x', y'))
            then (idx, col . brush $ w)
            else (idx, colr) in
      let newmap = Prelude.map updateIdx (worldMap w) in
        w {worldMap = newmap}

-- |[getPix p w] is the pixel of [w] corresponding to mouse position [p]
getPix :: Point -> World -> Either String (Int, Int)
getPix (fx, fy) w =
  let x = quot (round fx) (zoom w) + (quot (canvasWidth w) 2) in
    let y = quot (round (-1*fy)) (zoom w) + (quot (canvasHeight w) 2) in
      if (x > 0 && y > 0 && x < (canvasWidth w) && y < (canvasHeight w))
      then Right (x, y)
      else Left "Out of bounds"

-- |[stroke p w] is a world with [worldMap w] updated to include a mark of
--   color [col $ brush w] and width [sz $ brush w] at position [p]
stroke :: Point -> World -> World
stroke pos w =
    case (getPix pos w) of
    Right pix ->
      let updated = updateMarks pix w in
     makeMark (updated) pix
    Left x -> w

-- |[connectStrokes w] is a world where the last two strokes of [w] are visually
--   connected, if they exist.
connectStrokes :: World -> World
connectStrokes w =
  case (last4marks w) of
    p1:p2:t -> fillLine p1 p2 w
    p:[] -> makeMark w p
    _ -> w

-- |[fillLine p1 p2 w] is a world where the marks at points p1 and p2 are
--   visually connected by a straight line.
fillLine :: (Int, Int) -> (Int, Int) -> World -> World
fillLine (x1, y1) (x2, y2) w =
  if (abs (x2 - x1)) > (abs (y2 - y1))
  -- solve for y in terms of x
  then
    case (lineCoefficients (x1, y1) (x2, y2)) of
      Right (a, b) ->
        let rangelst = if x1 > x2 then [x1,x1-1 .. x2] else [x1 .. x2] in
          let pointlst = Prelude.map
                             (\x -> (x, round (a * (fromIntegral x) + b)))
                             rangelst in
                   (Prelude.foldl makeMark w pointlst)
      Left _ -> makeMark w (x1, y1)
  -- solve for x in terms of y
   else
     case lineCoefficients (y1, x1) (y2, x2) of
       Right (a, b) ->
        let rangelst = if y1 > y2 then [y1,y1-1 .. y2] else [y1 .. y2] in
          let pointlst = Prelude.map
                              (\y -> (round (a * (fromIntegral y) + b), y))
                              rangelst in
                 (Prelude.foldl makeMark w pointlst)
       Left _ -> makeMark w (x1, y1)

-- |[lineCoefficients p1 p2] is [(a, b}] such that a * x + b = y is the equation
--   of the line containing points [p1] and [p2].
lineCoefficients :: (Int, Int) -> (Int, Int) -> Either String (Float, Float)
lineCoefficients (x1, y1) (x2, y2) =
  if (x2 - x1) /= 0
  then
    let a = (fromIntegral (y2 - y1)) / (fromIntegral (x2 - x1)) in
      let b = (fromIntegral y1) - (a * (fromIntegral x1)) :: Float in
        Right (a, b)
  else
    Left "infinite slope"

-- |[toRepa w] is a representation of [w] as a 2-D delayed Repa array
toRepa :: World -> IO (R.Array D DIM2 Color)
toRepa w =
  let len = (canvasHeight w) * (canvasWidth w) in
  let sh =  (Z :. (canvasHeight w) :. (canvasWidth w)) in
    let colorList = (Prelude.map snd (worldMap w)) in
      let iArray = fromListUnboxed sh colorList in
    return (R.map (\(r, g, b) -> rgbI' r g b) iArray)

-- |Updates list of last 4 marks
updateMarks :: (Int, Int) -> World -> World
updateMarks pos w = w {last4marks = pos : (Prelude.take 3 (last4marks w))}

handleEvent :: Event -> World -> IO World

-- exit
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) _ =
  System.Exit.exitSuccess

-- make a mark if the mouse button is down change mouseState
handleEvent (EventKey (MouseButton LeftButton) Down _ pos) w =
  return (stroke pos (w {mouseState = MouseDown}))

-- clear last 4 marks list if mouse is up
handleEvent (EventKey (MouseButton LeftButton) Up _ pos) w =
  return (w {mouseState = MouseUp, last4marks = []})

-- draw if mouse is down
handleEvent (EventMotion pos) w =
  case (mouseState w) of
     MouseDown ->
       let nextspot = stroke pos w in
       -- return (stroke pos w)
         return (connectStrokes nextspot)
     MouseUp -> return w

-- dither patterns
handleEvent (EventKey (Char num) Down (Modifiers Up Down Up) _) w =
  let pattern =
        case num of
          '2' -> (\(x, y) -> (x + y) `mod` 2 == 0)
          '3' -> (\(x, y) -> x `mod` 2 == 0)
          '4' -> (\(x, y) -> (x - y) `mod` 4 == 0)
          '5' -> (\(x, y) -> (x - y) `mod` 4 == 0 && (x + y) `mod` 4 == 0)
          '6' -> (\(x, y) -> (x - y) `mod` 4 == 0 || (x + y) `mod` 4 == 0)
          '7' -> (\(x, y) -> if y `mod` 2 == 0
                             then x `mod` 4 == 0
                             else (x + 2) `mod` 4 == 0)
          '8' -> (\(x, y) -> y `mod` 2 == 0)
          '9' -> (\(x,y) -> (x - y) `mod` 4 == 0 || (x - y + 1) `mod` 4 == 0)
          '0' -> (\(x,y) -> (x - y) `mod` 3 == 0 || (x - y + 1) `mod` 4 == 0)
          _ -> (\_ -> True)
  in
  return (w {brush = ((brush w) {dither = pattern})})

-- change color
handleEvent (EventKey (Char num) Down (Modifiers Up Up Up) _) w | '0' <= num && num <= '9' =
  let idx = fromEnum num - fromEnum '0' in
    case ((palette w) VU.!? idx) of
      Just x -> return (w {brush = ((brush w) {col = x})})
      Nothing -> return w

-- increase brush size
handleEvent (EventKey (Char 'w') Down _ _) w =
  return (w {brush = ((brush w) {sz = (sz $ brush w) + 2})})

-- decrease brush size
handleEvent (EventKey (Char 'q') Down _ _) w =
  return (w {brush = ((brush w) {sz = max ((sz $ brush w) - 2) 2})})

-- clear canvas
handleEvent (EventKey (Char 'c') Down _ _) w =
  return (w {worldMap = zip [0..]
              (Prelude.replicate ((canvasWidth w)*(canvasHeight w)) (255, 255, 255))})

-- save
handleEvent (EventKey (Char 's') Down _ _) w =
  save w >> putStrLn ("Saved image to " Prelude.++ (filename w)) >> return w

handleEvent _ w = return w

step :: Float -> World -> IO World
step _ w = return w

-- |[loadImg fname] returns [(ht, wd, wm)], where [ht], [wd] and [wm] are the
--   height, width and pixel data of the image stored in [fname].
loadImg :: String -> IO (Int, Int, [(Int, Hue)])
loadImg fname = do
  readOut <- JP.readImage fname
  case readOut of
    Right di -> case (JP.Extra.flipVertically $ JP.convertRGB8 di) of
      Image wd ht vec ->
        let pixels = getPixels vec in
          return (ht, wd, pixels)
    Left _ -> putStrLn "invalid file" >> System.Exit.exitFailure

-- |[getPixels v] converts a vector containing JuicyPixels image data to
--   a worldmap.
getPixels :: VS.Vector GHC.Word.Word8 -> [(Int, Hue)]
getPixels v =
  let vlst = (VS.toList v) in
    let pixlst lst = case lst of
          r:g:b:t -> ((fromIntegral r, fromIntegral g, fromIntegral b):(pixlst t))
          _ -> [] in
      zip [0..] (pixlst vlst)

-- |[hueToPixel wm] converts a worldmap to a vector used to make a JuicyPixels
--   image.
hueToPixel :: [(Int, Hue)] -> VS.Vector GHC.Word.Word8
hueToPixel wm =
  let stripped = Prelude.map snd wm in
    let flattener (r, g, b) lst = r:g:b:lst in
      let flattened = Prelude.foldr flattener [] stripped in
        let wordlst = Prelude.map fromIntegral flattened :: [Word8] in
        VS.fromList wordlst

-- |[save w] writes the image represented by [w] to a file.
save :: World -> IO ()
save w =
  let i =
        JP.Extra.flipVertically
        (JP.Image (canvasWidth w) (canvasHeight w) (hueToPixel $ worldMap w)) in
    let di = ImageRGB8 i in
      let out = JP.Save.imageToPng di in
      B.writeFile (filename w) out

beginDrawNew :: C.Setup -> String -> IO()
beginDrawNew st fname =
  let wd = C.width st in
    let ht = C.height st in
      let wm = (zip [0..] (Prelude.replicate (wd*ht) ((C.palette st) VU.! 0))) in
        beginDraw wd ht wm st fname

-- |[beginDraw wd ht wm fname] starts the drawing program with a canvas of
--   width [wd], height [ht], worldmap [w], setup [st]  and filename [fname].
beginDraw :: Int -> Int -> [(Int, Hue)] -> C.Setup -> String -> IO()
beginDraw wd ht wm st fname =
  let zm = 4 in
    let startWorld =
         World { worldMap = wm,
                     brush = Brush {sz = 2,
                                    col = (0, 0, 0),
                                    dither = \_ -> True},
                     canvasHeight = ht,
                     canvasWidth = wd,
                     zoom = zm,
                     mouseState = MouseUp,
                     last4marks = [],
                     filename = fname,
                     palette = C.defaultPalette
               } in
  playArrayIO
  FullScreen
  (zm, zm)
  20
  startWorld
  toRepa
  handleEvent
  step

main :: IO ()
main =
  do
    args <- getArgs
    setup <- C.getConfig args
    putStrLn "Load existing image? (y/n)"
    resp <- getLine
    putStrLn "Enter filename:"
    fname <- getLine
    if (head resp == 'y') then
      do
        (htI, wdI, wmI) <- loadImg fname
        beginDraw wdI htI wmI C.dft fname
      else
        let wd = 150 in
          let ht = 100 in
            beginDraw
            wd
            ht
            (zip [0..] (Prelude.replicate (wd*ht) (255, 255, 255)))
            C.dft
            fname
