{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Aroma where

import Import
import Handler.Funcs as F

--OPTIONS--

optionsAromaR :: Handler ()
optionsAromaR = F.anyOriginIn [ F.OPTIONS, F.POST ]

-- POST --

postAromaR :: Handler Value
postAromaR = do
    aroma    <-  requireJsonBody :: Handler Aroma
    aromaId  <-  runDB $ insert aroma
    sendStatusJSON created201 $ object [ "resp" .= aromaId ]


