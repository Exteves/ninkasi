{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Funcs where

import Foundation
import Import
import qualified Prelude as P
import Data.Text as T

data HttpMethod = OPTIONS | GET | POST | PUT | PATCH | DELETE deriving Show

(%=.) :: EntityField record Text -> Text -> Filter record
(%=.) campo valor = Filter campo ( Left $ T.concat ["%",valor,"%"] )
                                 ( BackendSpecificFilter "ILIKE" )

anyOriginIn :: [HttpMethod] -> Handler ()
anyOriginIn methods = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" $ T.intercalate ", " $ P.map (T.pack.show) methods