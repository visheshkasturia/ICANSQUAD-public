module Image (main, deconstructTree, Pixel (..), PixelValue (..), overlay) where

import Codec.Picture
  ( DynamicImage (ImageRGB8),
    Image (Image, imageData, imageHeight, imageWidth),
    PixelRGB8 (PixelRGB8),
    convertRGB8,
    readImage,
    savePngImage,
  )
import qualified Codec.Picture.Types as CT
import Data.AEq
import qualified Data.Vector.Storable as V
import Quad
import System.IO
import Text.Read (readMaybe)

{-
===========
  -- Datatypes for Image operations
===========
-}

data PixelValue = Val Double Double Double deriving (Show, Eq)

data Pixel = P PixelValue Int deriving (Show, Eq)

instance Semigroup Pixel where
  (P (Val r g b) c) <> (P (Val r' g' b') c') = P (Val (avg r r') (avg g g') (avg b b')) (c + c')
    where
      avg a b = if denom == 0 then 0.0 else min 255 (max (numer a b / denom) 0)
      denom = fromIntegral (c + c')
      numer a b = fromIntegral c * a + fromIntegral c' * b

instance Monoid Pixel where
  mempty = P (Val 0 0 0) 0

valEqual :: PixelValue -> PixelValue -> Bool
valEqual (Val r g b) (Val r' g' b') = r ~== r' && g ~== g' && b ~== b'

instance AEq Pixel where
  (P v c) ~== (P v' c') = valEqual v v' && c == c'

{-
===========
  -- Some user input helper functions + state for user input
===========
-}
prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

promptInfo :: Maybe String -> IO String
promptInfo Nothing = prompt "Image > "
promptInfo (Just x) = prompt ("Image " ++ x ++ "> ")

promptImage :: State -> IO State
promptImage st = do
  putStrLn "No loaded image. Try loading an image first!"
  return st

promptTree :: State -> IO State
promptTree st = do
  putStrLn "Tree hasn't been built. Build a tree first!"
  return st

-- Stores user state when moving from feature to feature
data State = State
  { imagename :: Maybe String,
    img :: Maybe (Image PixelRGB8),
    qt :: Maybe (Q Pixel)
  }

initialState :: State
initialState =
  State
    { imagename = Nothing,
      img = Nothing,
      qt = Nothing
    }

{-
===========
-- Utility functions for some of the image operations
===========
-}

-- Convert the pixel type from JuicyPixels to our pixel
toPixel :: PixelRGB8 -> Pixel
toPixel (PixelRGB8 r g b) = P (Val (fromIntegral r) (fromIntegral g) (fromIntegral b)) 1

-- Sets up parameters for buildTree from Quad library to build a quadtree from image
-- with function parameter for different types of pixels
build :: Monoid e => Image PixelRGB8 -> (PixelRGB8 -> e) -> Q e
build image getPixel =
  buildTree
    (imageWidth image)
    (imageHeight image)
    (\x y -> Just (getPixel $ CT.pixelAt image x y))

-- Given a quadtree and a function that outputs a dlist for every node's value and box
-- return the entire 2d area of values for the quadtree
fillTree :: QuadTree e -> (e -> Box -> [[e] -> [e]]) -> [[e]]
fillTree tree fillNode = map ($ []) (fill tree)
  where
    fill E = []
    fill (Leaf b _ v) = fillNode v b
    fill (N _ _ tL tR bL bR) =
      let topHalf = zipDL (fill tL) (fill tR)
       in let bottomHalf = zipDL (fill bL) (fill bR)
           in if null topHalf || null (head topHalf [])
                then bottomHalf
                else
                  if null bottomHalf || null (head bottomHalf [])
                    then topHalf
                    else topHalf ++ bottomHalf

-- Create the 2d image stored in the tree itself and add lines if true
deconstructTree :: Bool -> Q e -> e -> [[e]]
deconstructTree addLines (QuadTree tree _) p = fillTree tree fillLeaf
  where
    fillLeaf v (Rect (xL, yL) (xT, yT)) =
      if addLines
        then replicateDL (height - 1) row [pRow]
        else replicate height (replicateDL width v)
      where
        height = countIntsBetween yL yT
        width = countIntsBetween xL xT
        row = replicateDL (width - 1) v . (p :)
        pRow = replicateDL width p

-- Takes a quadtree, a delimiter value, an indexing function like in buildTree that
-- takes a row and column and outputs the value to get the original image with
-- the quadtree nodes outlined on it (ie. an overlay)
overlay :: (Monoid e) => Q e -> e -> (Int -> Int -> Maybe e) -> [[e]]
overlay (QuadTree tree (Rect (xL, yL) (xT, yT))) p f =
  fillTree tree (const (\b -> fillBox b height p f))
  where
    height = countIntsBetween yL yT

-- Fills a single box with the values from the original image which the function
-- parameter gives access to
fillBox :: Box -> Int -> e -> (Int -> Int -> Maybe e) -> [[e] -> [e]]
fillBox (Rect (xL, yL) (xT, yT)) height p = rowInd lowerY upperY
  where
    bottomRow = replicateDL (countIntsBetween xL xT) p
    (lowerX, upperX) = (floor xL, floor xT - 2)
    (lowerY, upperY) = (floor yL + 2, floor yT)

    colInd l f r
      | r < l = id
      | otherwise = case f r of
        Nothing -> colInd l f (r - 1) . (p :)
        Just s -> colInd l f (r - 1) . (s :)

    rowInd lower upper f
      | upper < lower = [bottomRow]
      | otherwise = (colInd lowerX (f (height - upper)) upperX . (p :)) : rowInd lower (upper - 1) f

-- replicate for DLists
replicateDL :: Int -> e -> [e] -> [e]
replicateDL 0 a = id
replicateDL i a = replicateDL (i - 1) a . (a :)

-- zip with padding for DLists
zipDL :: [[e] -> [e]] -> [[e] -> [e]] -> [[e] -> [e]]
zipDL (x : xs) (y : ys) = x . y : zipDL xs ys
zipDL [] ys = ys
zipDL xs [] = xs

-- Convert the result of overlay or deconstruct into an image to write to disk
arrToImg :: [[Pixel]] -> Image PixelRGB8
arrToImg ls@(x : xs) =
  Image
    { imageData = V.fromList $ foldr consRow [] ls,
      imageWidth = length x,
      imageHeight = length ls
    }
  where
    consRow row acc = foldr consPixel acc row
    consPixel (P (Val r g b) _) acc = ceiling r : ceiling g : ceiling b : acc
arrToImg [] = Image {imageData = V.fromList [], imageWidth = 0, imageHeight = 0}

-- Provides an alternative equality predicate that's more aggressive in compression
boundedEquality :: Int -> Pixel -> Pixel -> Bool
boundedEquality a (P (Val r g b) _) (P (Val r' g' b') _) = isBound r r' && isBound g g' && isBound b b'
  where
    isBound x y
      | y < x = isBound y x
      | otherwise = (y - x) < fromIntegral a || (y - x) ~== fromIntegral a

-- Gets ratio of number of quadtree nodes / size of initial 2d array
compressionRatio :: Q Pixel -> Int -> Double
compressionRatio _ 0 = 0
compressionRatio qt size = fromIntegral (countNodes qt) / fromIntegral size

{-
===========
Functions for handling different user input:
===========
-}

{-
  -- Set Image - Tries reading input from given filepath
-}
setImage :: State -> IO State
setImage st = do
  input <- prompt "Type in path to image or (:q) to quit: "
  case input of
    ":q" -> return st
    _ ->
      do
        img <- readImage input
        case img of
          Left err -> do
            putStrLn ("Could not read image: " ++ err ++ "\nTry again!")
            setImage st
          Right dynImg -> do
            putStrLn "Successfully read image!"
            putStrLn "Converting image to RGB8"
            putStrLn ("Width: " ++ show (imageWidth converted))
            putStrLn ("Height: " ++ show (imageHeight converted))
            return (st {imagename = Just input, img = Just converted, qt = Nothing})
            where
              converted = convertRGB8 dynImg

{-
  -- Build tree - ensures there's an image set before building a tree and outputting tree info
-}
buildTreeFromImage :: State -> IO State
buildTreeFromImage st = do
  case img st of
    Nothing -> promptImage st
    Just image -> do
      putStrLn ("Built tree with " ++ show leafNodeCount ++ " leaf nodes successfully!")
      return st {qt = Just tree}
      where
        tree = build image toPixel
        leafNodeCount = countLeafNodes tree

{-
  -- Compress Image - functions to read input for compression bound, set up compress
  -- tree call to Quad library and output resulting tree info
-}
compressImage :: Image PixelRGB8 -> Q Pixel -> Int -> State -> IO State
compressImage im q cmp st = do
  putStrLn ("Compressed the image with " ++ show pixels ++ " pixels to " ++ show nodeCount ++ " node(s) successfully!")
  putStrLn ("Compression Ratio (nodes/pixels): " ++ show (compressionRatio tree pixels))
  return st {qt = Just tree}
  where
    tree =
      if cmp == 0
        then compressTree q (\(P v _) (P v' _) -> valEqual v v')
        else compressTree q (boundedEquality cmp)
    pixels = imageWidth im * imageHeight im
    nodeCount = countNodes tree

compressLoop :: Image PixelRGB8 -> Q Pixel -> State -> IO State
compressLoop im q st = loop st
  where
    loop st = do
      compMBound <- prompt "How much would you like to compress the image from 0 (lossless) - 255 (everything)? "
      case compMBound of
        ":q" -> return st
        _ ->
          case (readMaybe compMBound :: Maybe Int) of
            Nothing -> do
              putStrLn "We couldn't read that. Try again."
              loop st
            Just compBound ->
              if compBound > 255 || compBound < 0
                then do
                  putStrLn "Number out of bounds. Try again."
                  loop st
                else compressImage im q compBound st

handleCompress :: State -> IO State
handleCompress st = do
  case img st of
    Nothing -> promptImage st
    Just image ->
      case qt st of
        Nothing -> promptTree st
        Just q -> compressLoop image q st

{-
  -- Write Image -- functions to read output file and deconstruct tree into
  -- an image with the extra option of adding lines to it
-}
writeImage :: FilePath -> Q Pixel -> Bool -> IO ()
writeImage outputFile q addLines =
  savePngImage outputFile (ImageRGB8 (arrToImg (deconstructTree addLines q (P (Val 0 0 0) 1))))

writeImageLoop :: Q Pixel -> State -> IO State
writeImageLoop q st = loop st
  where
    loop st = do
      outputFile <- prompt "Where would you like to store the output image? "
      input <- prompt "Would you like to add lines around the tree nodes (y/n/:q)? "
      case input of
        ":q" -> return st
        "y" -> do
          writeImage outputFile q True
          return st
        "n" -> do
          writeImage outputFile q False
          return st
        _ -> do
          putStrLn "Didn't quite catch that. (:q) to quit. Try again."
          loop st

handleWriteImage :: State -> IO State
handleWriteImage st = do
  case img st of
    Nothing -> promptImage st
    Just image ->
      case qt st of
        Nothing -> promptTree st
        Just q -> writeImageLoop q st

{-
  -- Overlay -- functions to overlay image with the given output file
-}
overlayImage :: Image PixelRGB8 -> Q Pixel -> State -> IO State
overlayImage image q st = do
  outputFile <- prompt "Where would you like to store the output image? "
  case outputFile of
    ":q" -> return st
    _ -> do
      savePngImage
        outputFile
        (ImageRGB8 (arrToImg (overlay q (P (Val 0 0 0) 1) (\y x -> Just (toPixel $ CT.pixelAt image x y)))))
      return st

handleOverlay :: State -> IO State
handleOverlay st = do
  case img st of
    Nothing -> promptImage st
    Just image ->
      case qt st of
        Nothing -> promptTree st
        Just q -> overlayImage image q st

mainLoop :: IO ()
mainLoop = go initialState
  where
    go :: State -> IO ()
    go st = do
      input <- promptInfo (imagename st)
      case input of
        "1" -> do
          st' <- setImage st
          go st'
        "2" -> do
          st' <- buildTreeFromImage st
          go st'
        "3" -> do
          st' <- handleCompress st
          go st'
        "4" -> do
          st' <- handleWriteImage st
          go st'
        "5" -> do
          st' <- handleOverlay st
          go st'
        "6" -> do
          putStrLn "Cleared!"
          go st {imagename = Nothing, img = Nothing, qt = Nothing}
        ":q" -> return ()
        ":h" -> do
          instrs
          go st
        _ -> go st

intro :: IO ()
intro = do
  putStrLn "Welcome to the Image application!"

instrs :: IO ()
instrs = do
  putStrLn "Your options are: "
  putStrLn "(1) Input an image to transform"
  putStrLn "(2) Build tree from image"
  putStrLn "(3) Compress the image"
  putStrLn "(4) Output image from quadtree"
  putStrLn "(5) Overlay quadtree nodes on original image"
  putStrLn "(6) Clear image"
  putStrLn "(:q) To exit"
  putStrLn "(:h) To print this message"

main :: IO ()
main = do
  intro
  instrs
  mainLoop