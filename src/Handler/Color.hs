{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Color where

import Import
import Handler.Funcs as F

optionsColorR :: Handler ()
optionsColorR = F.anyOriginIn [ F.OPTIONS, F.POST, F.GET ]

-- POST --

postColorR :: Handler Value
postColorR = do
    F.anyOriginIn [ F.OPTIONS, F.POST ]
    color    <-  requireJsonBody :: Handler Color
    colorId  <-  runDB $ insert color
    sendStatusJSON created201 $ object [ "resp" .= colorId ]

getColorR :: Handler Value
getColorR = do
    F.anyOriginIn [ F.OPTIONS, F.GET ]
    colors   <-  runDB $ selectList [] [ Asc ColorName ]
    sendStatusJSON ok200 $ object [ "resp" .= colors ]
