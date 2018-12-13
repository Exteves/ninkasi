{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Aroma where

import Import
import Handler.Funcs as F

--OPTIONS--

optionsAromaR :: Handler ()
optionsAromaR = F.anyOriginIn [ F.OPTIONS, F.POST, F.GET ]

optionsAromaByIdR :: AromaId -> Handler ()
optionsAromaByIdR _ = F.anyOriginIn [ F.OPTIONS, F.GET, F.DELETE ]

-- POST --

postAromaR :: Handler Value
postAromaR = do
    F.anyOriginIn [ F.OPTIONS, F.POST ]
    aroma    <-  requireJsonBody :: Handler Aroma
    aromaId  <-  runDB $ insert aroma
    sendStatusJSON created201 $ object [ "resp" .= aromaId ]

getAromaR :: Handler Value
getAromaR = do
    F.anyOriginIn [ F.OPTIONS, F.GET ]
    smells   <-  runDB $ selectList [] [ Asc AromaName ]
    sendStatusJSON ok200 $ object [ "resp" .= smells ]

getAromaByIdR :: AromaId -> Handler Value
getAromaByIdR aromaId = do
    F.anyOriginIn [ F.OPTIONS, F.GET ]
    aroma   <-  runDB $ get404 aromaId
    sendStatusJSON ok200 $ object [ "resp" .= aroma ]

deleteAromaByIdR :: AromaId -> Handler Value
deleteAromaByIdR aromaId = do
    F.anyOriginIn [ F.OPTIONS, F.DELETE ]
    runDB $ delete aromaId
    sendStatusJSON ok200 $ object ["resp" .= ("ok"::Text)]