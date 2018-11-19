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
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" $ T.intercalate ", " $ P.map (T.pack.show) methods

getTokenHeader :: Handler Text
getTokenHeader = do
    a <- waiRequest
    listaHeader <- return $ NW.requestHeaders a
    return $ T.pack $ BS.unpack $ M.fromJust $ P.lookup "key" listaHeader
    