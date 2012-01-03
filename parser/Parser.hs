module Parser where

import Prelude hiding (catch)

import Control.Applicative
import Control.Exception

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import Foreign.C.String

import Network.URI hiding (path)
import Network.HTTP (openStream, sendHTTP, getResponseBody, HandleStream, Request(..), RequestMethod(..), Header(..), HeaderName(..))

import ParseLog

foreign export stdcall cGamesAsJSON :: CString -> IO CString
foreign export stdcall parseFile :: CString -> IO CString
foreign export stdcall uploadLog :: CString -> IO CString


cGamesAsJSON :: CString -> IO CString
cGamesAsJSON input = do
    jsonBS <- gamesAsJSON <$> BS.packCString input
    newCString . C8.unpack $ jsonBS
    
parseFile :: CString -> IO CString
parseFile cInput = do
    let cOutput = peekCString cInput
                >>= BS.readFile
                >>= newCString . C8.unpack . gamesAsJSON
    catch (cOutput)
          (\(SomeException e) -> newCString . show $ e)
    

uploadLog :: CString -> IO CString
uploadLog cInput = handle
    (\(SomeException e) -> newCString . show $ e) $
    do
        fileName <- peekCString cInput
        json <- gamesAsJSON <$> BS.readFile fileName
        conn <- openStream "lol.casualaddict.com" 80
        uploadGame conn json
        newCString "Success"


uploadGame :: HandleStream BS.ByteString -> BS.ByteString -> IO ()
uploadGame conn jsonGames = do
    rawResponse <- sendHTTP conn request
    respBody <- getResponseBody rawResponse
    return ()
    where
        uri = maybe undefined id $ parseURI "http://lol.casualaddict.com/game"
        request = Request { rqURI = uri
                          , rqMethod = POST
                          , rqHeaders = [ Header HdrContentType "application/json; charset=utf-8"
                                        , Header HdrContentLength (show $ BS.length jsonGames)
                                        ]
                          , rqBody = jsonGames
                          }
