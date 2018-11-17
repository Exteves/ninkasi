{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Color where

import Import
import Handler.Funcs as F

optionsColorR :: Handler ()
optionsColorR = F.anyOriginIn [ F.OPTIONS, F.POST ]

-- POST --

postColorR :: Handler Value
postColorR = do
    color    <-  requireJsonBody :: Handler Color
    colorId  <-  runDB $ insert color
    sendStatusJSON created201 $ object [ "resp" .= colorId ]


