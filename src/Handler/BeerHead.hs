{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.BeerHead where

import Import
import Handler.Funcs as F

optionsBeerHeadR :: Handler ()
optionsBeerHeadR = F.anyOriginIn [ F.OPTIONS, F.POST ]

-- POST --

postBeerHeadR :: Handler Value
postBeerHeadR = do
    beerhead    <-  requireJsonBody :: Handler BeerHead
    beerheadId  <-  runDB $ insert beerhead
    sendStatusJSON created201 $ object [ "resp" .= beerheadId ]


