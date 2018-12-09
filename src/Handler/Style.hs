{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Style where

import Import
import Handler.Funcs as F

optionsStyleR :: Handler ()
optionsStyleR = F.anyOriginIn [ F.OPTIONS, F.POST, F.GET ]

-- POST --

postStyleR :: Handler Value
postStyleR = do
    F.anyOriginIn [ F.OPTIONS, F.POST ]
    style    <-  requireJsonBody :: Handler Style
    styleId  <-  runDB $ insert style
    sendStatusJSON created201 $ object [ "resp" .= styleId ]

getStyleR :: Handler Value
getStyleR = do
    F.anyOriginIn [ F.OPTIONS, F.GET ]
    styles   <-  runDB $ selectList [] [ Asc StyleName ]
    sendStatusJSON ok200 $ object [ "resp" .= styles ]


