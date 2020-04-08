{-# LANGUAGE OverloadedStrings #-}

module Config () where

import Data.Bits
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Vector.Unboxed as V
import qualified Text.Megaparsec.Char.Lexer as L

data Filetype = PNG | BMP | TIFF

data Setup = Setup { height :: Int,
                     width :: Int,
                     palette :: V.Vector (Int, Int, Int),
                     filetype :: Filetype }

type Parser = Parsec Void String

defaultPalette :: V.Vector (Int, Int, Int)
defaultPalette = V.fromList [(255, 255, 255),
                             (0, 0, 0),
                             (255, 0, 0),
                             (0, 255, 0),
                             (0, 0, 255),
                             (255, 255, 0),
                             (255, 0, 255),
                             (0, 255, 255)]
dft :: Setup
dft = Setup { height = 100,
                  width = 150,
                  palette = defaultPalette,
                  filetype = PNG }

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol' sc

pFormat :: Parser Filetype
pFormat = do
  _ <- symbol "filetype:"
  ft <- choice [ PNG  <$ symbol "png",
                 BMP  <$ symbol "bmp",
                 TIFF <$ symbol "tiff"]
  return ft

pWidth :: Parser Int
pWidth = do
  _ <- symbol "height:"
  lexeme L.decimal

pHeight :: Parser Int
pHeight = do
  _ <- symbol "format:"
  lexeme L.decimal

pColor :: Parser (Int, Int, Int)
pColor = do
  _ <- symbol "Color"
  n <- lexeme L.decimal
  if n > 9
    then fail "Can only specify colors 1-9"
    else do
    _ <- symbol ":"
    _ <- symbol "#"
    hex <- lexeme L.hexadecimal
    return (hexToRGB hex)

pPalette :: Parser (V.Vector (Int, Int, Int))
pPalette = do
  lst <- manyTill pColor eof
  return (V.fromList lst)

hexToRGB :: Int -> (Int, Int, Int)
hexToRGB h =
  let b = h `mod` 256 in
    let g = (shift h 8) `mod` 256 in
      let r = (shift h 16) `mod` 256 in
        (r, g, b)

pSetup :: Parser Setup
pSetup = do
  fmt <- pFormat
  ht <- pHeight
  wd <- pWidth
  pal <- pPalette
  return (Setup { height = ht,
                  width = wd,
                  palette = pal,
                  filetype = fmt })

configFromFile :: String -> IO (Setup)
configFromFile x = do
  hl <- openFile x ReadMode
  ct <- hGetContents hl
  hclose;
  case (runParser x ct) of
    Left p -> print p >> return dft
    Right r -> return r

getConfig :: [String] -> IO (Setup)
getConfig args =
  case args of
    [] -> return dft
    x::xs -> configFromFile x
