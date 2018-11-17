{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Province where

import Import
import Handler.Funcs as F

optionsProvinceR :: Handler ()
optionsProvinceR = F.anyOriginIn [ F.OPTIONS, F.POST ]

-- POST --

postProvinceR :: Handler Value
postProvinceR = do
    province    <-  requireJsonBody :: Handler Province
    provinceId  <-  runDB $ insert province
    sendStatusJSON created201 $ object [ "resp" .= provinceId ]


