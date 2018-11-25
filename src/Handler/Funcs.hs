{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Funcs where

import Foundation
import Import
import qualified Prelude as P
import Data.Text as T
import Network.Wai as NW
import qualified Data.Maybe as M
import qualified Data.ByteString.Char8 as BS

data HttpMethod = OPTIONS | GET | POST | PUT | PATCH | DELETE deriving Show

(%=.) :: EntityField record Text -> Text -> Filter record
(%=.) campo valor = Filter campo ( Left $ T.concat ["%",valor,"%"] )
                                 ( BackendSpecificFilter "ILIKE" )

anyOriginIn :: [HttpMethod] -> Handler ()
anyOriginIn methods = do
    addHeader (T.pack "Access-Control-Allow-Origin") (T.pack "*")
    addHeader (T.pack "Access-Control-Allow-Methods") $ T.intercalate (T.pack ", ") $ P.map T.pack $ P.map P.show methods
    addHeader (T.pack "Access-Control-Allow-Headers") (T.pack "*")

getTokenHeader :: Handler Text
getTokenHeader = do
    a <- waiRequest
    tastingHeader <- return $ NW.requestHeaders a
    return $ T.pack $ BS.unpack $ M.fromJust $ P.lookup "key" tastingHeader
    