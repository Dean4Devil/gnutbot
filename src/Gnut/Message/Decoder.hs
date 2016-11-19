module Gnut.Message.Decoder
    ( decode
    ) where

import           Control.Applicative                ((<$>), (<|>))
import qualified Data.Attoparsec.ByteString         as A
import qualified Data.Attoparsec.ByteString.Char8   as AC
import qualified Data.ByteString.Char8              as B
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T

import Gnut.Message

-- IRC has a very specific set of what is and is not a space (tabs for example are not.)
isSpace :: Char -> Bool
isSpace ' '  = True
isSpace '\n' = True
isSpace '\r' = True
isSpace _    = False

prefixParser :: A.Parser Prefix
prefixParser = do
    _ <- AC.char8 ':' -- Prefix is prefixed with a single colon
    prefix <- AC.takeTill $ \x -> isSpace x || x == '!' || x == '@'
    if '.' `B.elem` prefix
        then AC.skipWhile isSpace >> return (ServerPrefix $ T.decodeUtf8 prefix)
        else do
            user <- A.option Nothing $ Just <$> do
                _ <- AC.char8 '!'
                AC.takeTill $ \x -> isSpace x || x == '@'
            host <- A.option Nothing $ Just <$> do
                _ <- AC.char8 '@'
                AC.takeTill $ \x -> isSpace x
            AC.skipWhile isSpace
            return $ NickPrefix
                (T.decodeUtf8 prefix)
                (T.decodeUtf8 <$> user)
                (T.decodeUtf8 <$> host)

commandParser :: A.Parser Text
commandParser = T.decodeUtf8 <$> AC.takeTill isSpace

parameterParser :: A.Parser Text
parameterParser = do
    AC.skipWhile isSpace
    T.decodeUtf8 <$> (trailing <|> middle)
  where trailing = AC.char8 ':' >> AC.takeTill (\x -> x == '\r' || x == '\n')
        middle = AC.takeTill isSpace

messageParser :: A.Parser Message
messageParser = do
    prefix     <- A.option Nothing $ Just <$> prefixParser
    command    <- commandParser
    parameters <- A.manyTill parameterParser A.endOfInput
    return $ Message prefix command parameters

decode :: B.ByteString -> Maybe Message
decode = unwrap . A.parse messageParser
    where
        unwrap (A.Done _ x)  = Just x
        unwrap (A.Fail _ _ _)  = Nothing
        unwrap (A.Partial f) = case f mempty of
            A.Done _ x -> Just x
            _          -> Nothing
