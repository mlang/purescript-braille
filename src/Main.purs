module Main where

import Prelude hiding (between)
import Affjax (get, URL)
import Affjax.ResponseFormat (string, printResponseFormatError)
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (throwError)
import Control.Alternative ((<|>), empty)
import Control.Lazy (fix)
import Data.Array ((..), many, some)
import Data.Char (fromCharCode, toCharCode)
import Data.Either (either)
import Data.Enum (enumFromTo)
import Data.Foldable (elem, foldr, foldM, intercalate)
import Data.Int.Bits (shl, and, or)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.String (toLower)
import Data.String.CodeUnits (fromCharArray)
import Data.Char.Unicode (digitToInt, isSpace)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Text.Parsing.Parser (ParserT(..), consume, fail, runParserT)
import Text.Parsing.Parser.Combinators ((<?>), between, lookAhead, option, optional, skipMany, try, tryRethrow)
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token

flatten :: String -> Aff (Array Directive)
flatten name = eval name mempty where
  eval n ds = getTextTable n >>= (_ `parse` directives) >>= foldM f ds
  f ds (Include file) = eval file ds
  f ds d = pure $ ds <> pure d

table :: String -> Aff TextTable
table name = eval name mempty where
  eval n t = getTextTable n >>= (_ `parse` directives) >>= foldM f t
  f tbl (Alias c d) = pure $ tbl { aliases = Map.insert c d tbl.aliases }
  f tbl (Char c d) = pure $ tbl { chars = Map.insert c d tbl.chars }
  f tbl (Glyph c d) = pure $ tbl { glyphs = Map.insert c d tbl.glyphs }
  f tbl (Include f) = eval f tbl
  f tbl (IfCell inv c th el)
    | (if inv then not else identity) $ elem c (Map.values tbl.chars) || elem c (Map.values tbl.glyphs)
    = foldM f tbl th
    | otherwise
    = foldM f tbl el
  f tbl (IfGlyph inv c th el)
    | (if inv then not else identity) $ Map.member c tbl.chars || Map.member c tbl.glyphs
    = foldM f tbl th
    | otherwise
    = foldM f tbl el

type Dots = Int
type TextTable =
  { chars :: Map Char Dots
  , glyphs :: Map Char Dots
  , aliases :: Map Char Char
  }
 
character :: Parser Char
character = (char '\\' *> escaped) <|> (satisfy isChar <?> "c") where
  isChar c = not $ c == '#' || isSpace c
  escaped = char 'b' $> '\b'
        <|> char 'f' $> '\f'
        <|> char 'n' $> '\n'
        <|> char 'o' *> oct3
        <|> char 'r' $> '\r'
        <|> char 's' $> ' '
        <|> char 't' $> '\t'
        <|> char 'u' *> hex4
        <|> char 'U' *> hex8
        <|> char 'v' $> '\v'
        <|> char 'x' *> hex2
        <|> char 'X' *> hex2
        <|> char '#' $> '#'
        <|> char '\\' $> '\\'
  toInt p = digitToInt <$> p >>= maybe (fail "digitToInt") pure
  fromChar p = fromCharCode <$> p >>= maybe (fail "fromCharCode") pure
  hexInt = toInt hexDigit
  octInt = toInt octDigit
  hex2 = fromChar $ toInt2 16 <$> hexInt <*> hexInt
  oct3 = fromChar $ toInt3 8 <$> octInt <*> octInt <*> octInt
  hex4 = fromChar $ toInt4 16 <$> hexInt <*> hexInt <*> hexInt <*> hexInt
  hex8 = fromChar $ toInt8 16 <$> hexInt <*> hexInt <*> hexInt <*> hexInt
                              <*> hexInt <*> hexInt <*> hexInt <*> hexInt
  toInt2 base a b = base * a + b
  toInt3 base a b c = base * toInt2 base a b + c
  toInt4 base a b c d = base * toInt3 base a b c + d
  toInt8 base a b c d e f g h = base * base * base * base * toInt4 base a b c d
                              + toInt4 base e f g h

dots :: Parser Dots
dots = between ws ws $ z <|> someDots where
  z = char '0' $> zero
  someDots = map (foldr or zero) $ some dot <|> parenthesized many dot
  ws = void <<< many $ oneOf [' ', '\t']
  parenthesized p = between lparen rparen <<< between ws ws <<< p <<< between ws ws
  lparen = void $ char '('
  rparen = void $ char ')'
  dot = oneOf (enumFromTo '1' '8') <#> \c ->
    one `shl` (toCharCode c - toCharCode '1')

str :: Parser String
str = fromCharArray <$> many character

str1 :: Parser String
str1 = fromCharArray <$> some character

skipComment :: Parser Unit
skipComment = char '#' *> void (many (noneOf ['\n', '\r']))

skip :: Parser Unit
skip = void $ many (skipComment <|> void (satisfy isSpace))

skipSpace1 = void <<< some $ satisfy hspace where
  hspace ' ' = true
  hspace '\t' = true
  hspace  _ = false

keyword :: String -> Parser Unit
keyword match = skip *> (str1 >>= cmp) *> void (many (oneOf [' ', '\t'])) where
  cmp s | toLower s == match = pure unit
        | otherwise  = fail $ "Unexpected '" <> s <> "', expecting '" <> match <> "'"

data Directive = Char Char Dots
               | Glyph Char Dots
               | Alias Char Char
               | Include String
               | IfGlyph Boolean Char (Array Directive) (Array Directive)
               | IfCell Boolean Dots (Array Directive) (Array Directive)

instance showDirective :: Show Directive where
  show (Include str) = "include " <> str
  show (Char c d) = "char " <> show c <> " " <> show d
  show (Glyph c d) = "glyph " <> show c <> " " <> show d
  show (Alias c d) = "alias " <> show c <> " " <> show d
  show (IfCell inv c th el) =
    let name = if inv then "ifNotCell" else "ifCell"
    in name <> " " <> show c <> case th, el of
         [t], [] -> " " <> show t
         _, [] -> "\n" <> showDirectives th <> "\nendif"
         _, _ -> "\n" <> showDirectives th <> "\nelse\n" <> showDirectives el <> "\nendIf"
  show (IfGlyph inv c th el) =
    let name = if inv then "ifNotGlyph" else "ifGlyph"
    in name <> " " <> show c <> case th, el of
         [t], [] -> " " <> show t
         _, [] -> "\n" <> showDirectives th <> "\nendif"
         _, _ -> "\n" <> showDirectives th <> "\nelse\n" <> showDirectives el <> "\nendIf"

showDirectives :: Array Directive -> String
showDirectives = intercalate "\n" <<< map show

directive :: Parser Directive
directive = fix \p ->
  between skip skip $ (toLower <$> str1) >>= case _ of
    "include" -> Include <$> (skipSpace1 *> str1)
    "char" -> Char <$> (skipSpace1 *> character) <*> dots
    "glyph" -> Glyph <$> (skipSpace1 *> character) <*> dots
    "alias" -> Alias <$> (skipSpace1 *> character) <*> (skipSpace1 *> character)
    "ifcell" -> IfCell false <$> dots <*> some (try p) <*> option [] (keyword "else" *> some (try p)) <* keyword "endif"
    "ifnotglyph" -> IfGlyph true <$> (skipSpace1 *> character) <*> some (try p) <*> option [] (try $ keyword "else" *> some p) <* keyword "endif"
    "ifglyph" -> do
      glyph <- skipSpace1 *> character
      (lookAhead eol *> (IfGlyph false glyph <$> some (try p) <*> option [] (try $ keyword "else" *> some p) <* keyword "endif")) <|>
      ((\th -> IfGlyph false glyph [th] []) <$> p)
    other -> fail $ "Unexpected '" <> other <> "'"

eol :: Parser Unit
eol = skipMany (oneOf [' ', '\t']) *> optional skipComment *> void (oneOf ['\n','\r'])

directives :: Parser (Array Directive)
directives = many directive

type Parser = ParserT String Aff

parse :: String -> Parser ~> Aff
parse input parser = runParserT input parser >>= either throwParseError pure

baseURL :: URL
baseURL = "https://raw.githubusercontent.com/brltty/brltty/master/Tables/Text/"

getTextTable :: String -> Aff String
getTextTable file = do
  resp <- get string (baseURL <> file)
  case resp.status of
    StatusCode 200 -> either throwResponseFormatError pure resp.body
    StatusCode code -> throwError (error $ show code)

throwResponseFormatError = throwError <<< error <<< printResponseFormatError
throwParseError = throwError <<< error <<< show

main :: Effect Unit
main = launchAff_ $ getTextTable "de.ttb" >>= \s ->
  parse s directives >>= liftEffect <<< log <<< showDirectives

