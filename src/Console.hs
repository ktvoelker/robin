
module Console where

import Control.Applicative
import Control.Monad.Trans.Resource
import Control.Monad.State
import Data.Lens
import Data.Ratio
import Data.Word
import Graphics.Vty hiding ((<|>))
import qualified Graphics.Vty

import Console.Types

beside :: Image -> Image -> Image
beside = (Graphics.Vty.<|>)

infixr 5 `beside`

type CM a = StateT Console (ResourceT IO) a

runCM :: CM a -> IO a
runCM m = runResourceT $ do
  (key, vty) <- allocate mkVty shutdown
  evalStateT m (mkConsole vty) <* release key

data Align = Start | CenterStart | CenterEnd | End

type Translator = Int -> Image -> Image

type Padder = Word -> Image -> Image

type Cropper = Word -> Image -> Image

type Sizer = Image -> Word

wordToInt :: Word -> Int
wordToInt w
  | w > fromIntegral (maxBound :: Int) = error "wordToInt: overflow"
  | otherwise = fromIntegral w

intToWord :: Int -> Word
intToWord z
  | z < 0 = error "intToWord: negative"
  | otherwise = fromIntegral z

fit1 :: Translator -> Padder -> Cropper -> Sizer -> Align -> Word -> Image -> Image
fit1 trans pad crop sizer align avail img =
  pad padExcess . trans (wordToInt transExcess) . crop avail $ img
  where
    size = sizer img
    excess = avail - (min size avail)
    (smallerHalfRaw, halfMod) = excess `divMod` 2
    smallerHalf = min 1 smallerHalfRaw
    biggerHalf = min 1 $ smallerHalf + halfMod
    (transExcess, padExcess) = case align of
      Start -> (0, excess)
      CenterStart -> (smallerHalf, biggerHalf)
      CenterEnd -> (biggerHalf, smallerHalf)
      End -> (excess, 1)

fitHoriz = fit1 (translate . (,0)) (pad . (,1)) (crop . (,maxBound)) image_width

fitVert = fit1 (translate . (0,)) (pad . (1,)) (crop . (maxBound,)) image_height

fit :: Align -> Align -> Word -> Word -> Image -> Image
fit ha va hb vb img = fitHoriz ha hb . fitVert va vb $ img

nextEvent :: CM Event
nextEvent = access cVty >>= liftIO . next_event

render :: CM Picture
render = do
  sw <- screenWidth
  let half = sw `div` 2
  title <- pad (half, 1) <$> access iTitle
  progress <- access iProgress
  sh <- screenHeight
  -- TODO crop body
  body <- pad (sw - 1, sh - 1) . crop (sw - 1, sh - 1) <$> access iBody
  scroll <- access iScroll
  return . pic_for_image $ (title `beside` progress) <-> (body `beside` scroll)

rebuild :: CM ()
rebuild = access cVty >>= \vty -> render >>= liftIO . update vty

screenSize :: CM DisplayRegion
screenSize = access cVty >>= display_bounds . terminal

screenWidth :: CM Word
screenWidth = region_width <$> screenSize

screenHeight :: CM Word
screenHeight = region_height <$> screenSize

setTitle :: String -> CM ()
setTitle xs = do
  void $ cTitle ~= xs
  void $ iTitle ~= string def_attr xs
  rebuild

clamp :: (Num a, Ord a) => a -> a
clamp n | n < 0     = 0
        | n > 1     = 1
        | otherwise = n

progressWidth :: Rational -> Word -> Word
progressWidth progress avail = round $ clamp progress * toRational avail

leftProgressDelim = char def_attr '['

rightProgressDelim = char def_attr ']'

renderProgress :: Rational -> Word -> Image
renderProgress progress avail =
  leftProgressDelim
  `beside` char_fill def_attr '=' complete 1
  `beside` char_fill def_attr '·' incomplete 1
  `beside` rightProgressDelim
  where
    avail' = round $ (toRational avail / 2) - 2
    complete = progressWidth progress avail'
    incomplete = avail' - complete

setProgress :: (Real a) => Maybe a -> CM ()
setProgress progress = do
  void $ cProgress ~= progress'
  void $
    screenWidth
    >>= (iProgress ~=)
        . maybe (const empty_image) renderProgress progress'
  rebuild
  where
    progress' = toRational <$> progress

paragraph :: Attr -> [String] -> Image
paragraph attr = foldr (<->) empty_image . map (line attr)

line :: Attr -> String -> Image
line attr "" = string attr " "
line attr xs = string attr xs

renderBody :: String -> CM ()
renderBody xs = do
  sh <- screenHeight
  let ls = lines xs
  void $ cBody ~= ls
  void $ iBody ~= paragraph def_attr (take (wordToInt $ sh - 1) ls)

scrollGapDisplaySize :: (Integral a, Integral b) => Word -> a -> b -> Word
scrollGapDisplaySize avail gapLines totalLines =
  fromInteger $ max (min n 1) $ round (clamp (n % d) * toRational avail)
  where
    n = fromIntegral gapLines
    d = fromIntegral totalLines

calcScroll :: Word -> Word -> Word -> (Word, Word, Word)
calcScroll avail preLines postLines = (preDisplay, visDisplay, postDisplay)
  where
    visLines = min postLines avail
    totalLines = preLines + postLines
    preDisplay = scrollGapDisplaySize avail preLines totalLines
    postDisplay = scrollGapDisplaySize avail (postLines - visLines) totalLines
    visDisplay = avail - preDisplay - postDisplay

renderScroll :: CM ()
renderScroll = do
  avail <- (subtract 1) <$> screenHeight
  preLines <- intToWord . length <$> access cBodyPrev
  postLines <- intToWord . length <$> access cBody
  let (preDisplay, visDisplay, postDisplay) = calcScroll avail preLines postLines
  let visImg = char_fill def_attr '|' 1 visDisplay
  let img = pad (1, postDisplay) $ translate (0, wordToInt preDisplay) $ visImg
  void $ iScroll ~= img

setBody :: String -> CM ()
setBody xs = do
  void $ cBodyPrev ~= []
  renderBody xs
  renderScroll
  rebuild

