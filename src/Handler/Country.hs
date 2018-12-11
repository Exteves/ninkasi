{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Country where

import Import
import Handler.Funcs as F

optionsCountryR :: Handler ()
optionsCountryR = F.anyOriginIn [ F.OPTIONS, F.POST, F.GET ]

optionsCountryByIdR :: CountryId -> Handler ()
optionsCountryByIdR _ = F.anyOriginIn [ F.OPTIONS, F.GET ]

-- POST --

postCountryR :: Handler Value
postCountryR = do
    F.anyOriginIn [ F.OPTIONS, F.POST ]
    country    <-  requireJsonBody :: Handler Country
    countryId  <-  runDB $ insert country
    sendStatusJSON created201 $ object [ "resp" .= countryId ]

getCountryR :: Handler Value
getCountryR = do
    F.anyOriginIn [ F.OPTIONS, F.GET ]
    countries   <-  runDB $ selectList [] [ Asc CountryName ]
    sendStatusJSON ok200 $ object [ "resp" .= countries ]

getCountryByIdR :: CountryId -> Handler Value
getCountryByIdR countryId = do
    F.anyOriginIn [ F.OPTIONS, F.GET ]
    country   <-  runDB $ get404 countryId
    sendStatusJSON ok200 $ object [ "resp" .= country ]