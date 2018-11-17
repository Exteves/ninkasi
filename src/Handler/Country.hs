{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Country where

import Import
import Handler.Funcs as F

optionsCountryR :: Handler ()
optionsCountryR = F.anyOriginIn [ F.OPTIONS, F.POST ]

-- POST --

postCountryR :: Handler Value
postCountryR = do
    country    <-  requireJsonBody :: Handler Country
    countryId  <-  runDB $ insert country
    sendStatusJSON created201 $ object [ "resp" .= countryId ]


