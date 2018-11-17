{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Style where

import Import
import Handler.Funcs as F

optionsStyleR :: Handler ()
optionsStyleR = F.anyOriginIn [ F.OPTIONS, F.POST ]

-- POST --

postStyleR :: Handler Value
postStyleR = do
    style    <-  requireJsonBody :: Handler Style
    styleId  <-  runDB $ insert style
    sendStatusJSON created201 $ object [ "resp" .= styleId ]


