{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Beer where

import Import
import Handler.Funcs
import Handler.Funcs as F

optionsBeerR :: Handler ()
optionsBeerR = F.anyOriginIn [ F.OPTIONS, F.POST ]

-- POST --

postBeerR :: Handler Value
postBeerR = do
    beer    <-  requireJsonBody :: Handler Beer
    beerId  <-  runDB $ insert beer
    sendStatusJSON created201 $ object [ "resp" .= beerId ]


