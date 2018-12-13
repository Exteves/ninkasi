{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Beer where

import Import
import Handler.Funcs as F

optionsBeerR :: Handler ()
optionsBeerR = F.anyOriginIn [ F.OPTIONS, F.POST, F.GET ]

optionsBeerByNameR :: Text -> Handler ()
optionsBeerByNameR _ = F.anyOriginIn [ F.OPTIONS, F.GET ]

optionsBeerByBreweryR :: Text -> Handler ()
optionsBeerByBreweryR _ = F.anyOriginIn [ F.OPTIONS, F.GET ]

optionsBeerByOriginR :: CountryId -> Handler ()
optionsBeerByOriginR _ = F.anyOriginIn [ F.OPTIONS, F.GET ]

optionsBeerByStyleIdR :: StyleId -> Handler ()
optionsBeerByStyleIdR _ = F.anyOriginIn [ F.OPTIONS, F.GET ]

optionsBeerByAromaIdR :: AromaId -> Handler ()
optionsBeerByAromaIdR _ = F.anyOriginIn [ F.OPTIONS, F.GET ]

optionsBeerByFlavorIdR :: FlavorId -> Handler ()
optionsBeerByFlavorIdR _ = F.anyOriginIn [ F.OPTIONS, F.GET ]

optionsBeerByColorIdR :: ColorId -> Handler ()
optionsBeerByColorIdR _ = F.anyOriginIn [ F.OPTIONS, F.GET ]

optionsBeerByIdR :: BeerId -> Handler ()
optionsBeerByIdR _ = F.anyOriginIn [ F.OPTIONS, F.GET, F.DELETE ]

-- POST --

postBeerR :: Handler Value
postBeerR = do
    F.anyOriginIn [ F.OPTIONS, F.POST ]
    beer    <-  requireJsonBody :: Handler Beer
    beerId  <-  runDB $ insert beer
    sendStatusJSON created201 $ object [ "resp" .= beerId ]

-- GET --

getBeerR :: Handler Value
getBeerR = do
    F.anyOriginIn [ F.OPTIONS, F.GET ]
    beerList    <-  runDB $ selectList [] [Asc BeerName]
    sendStatusJSON ok200 $ object [ "resp" .= beerList ]

    -- RESP WITHOUT ID EXAMPLE
    -- sendStatusJSON ok200 $ object [ "resp" .= (map entityVal beerList) ]

getBeerByNameR :: Text -> Handler Value
getBeerByNameR beerName = do
    F.anyOriginIn [ F.OPTIONS, F.GET ]
    beerList    <-  runDB $ selectList [ BeerName %=. beerName ] [ Asc BeerName ]
    beerListM   <-  return $ map entityKey beerList
    sendStatusJSON ok200 $ object [ "resp" .= beerListM ]

getBeerByBreweryR :: Text -> Handler Value
getBeerByBreweryR brewery = do
    F.anyOriginIn [ F.OPTIONS, F.GET ]
    beerList    <- runDB $ selectList [ BeerBrewery %=. brewery ] [ ]
    beerListM   <- return $ map entityKey beerList
    sendStatusJSON ok200 $ object [ "resp" .= beerListM ]

getBeerByOriginR :: CountryId -> Handler Value
getBeerByOriginR countryid = do
    F.anyOriginIn [ F.OPTIONS, F.GET ]
    beerList    <- runDB $ selectList [ BeerCountryOrigin ==. countryid ] [ ]
    beerListM   <- return $ map entityKey beerList
    sendStatusJSON ok200 $ object [ "resp" .= beerListM ]

getBeerByStyleIdR :: StyleId -> Handler Value
getBeerByStyleIdR styleid = do
    F.anyOriginIn [ F.OPTIONS, F.GET ]
    beerList    <- runDB $ selectList [ BeerStyle ==. styleid ] [ ]
    beerListM   <- return $ map entityKey beerList
    sendStatusJSON ok200 $ object [ "resp" .= beerListM ]

getBeerByAromaIdR :: AromaId -> Handler Value
getBeerByAromaIdR aromaid = do
    F.anyOriginIn [ F.OPTIONS, F.GET ]
    beerList    <- runDB $ selectList [ BeerAroma ==. aromaid ] []
    beerListM   <- return $ map entityKey beerList
    sendStatusJSON ok200 $ object [ "resp" .= beerListM ]

getBeerByFlavorIdR :: FlavorId -> Handler Value
getBeerByFlavorIdR flavorid = do
    F.anyOriginIn [ F.OPTIONS, F.GET ]
    beerList    <- runDB $ selectList [ BeerFlavor ==. flavorid ] []
    beerListM   <- return $ map entityKey beerList
    sendStatusJSON ok200 $ object [ "resp" .= beerListM ]

getBeerByColorIdR :: ColorId -> Handler Value
getBeerByColorIdR colorid = do
    F.anyOriginIn [ F.OPTIONS, F.GET ]
    beerList    <- runDB $ selectList [ BeerColor ==. colorid ] []
    beerListM   <- return $ map entityKey beerList
    sendStatusJSON ok200 $ object [ "resp" .= beerListM ]

getBeerByIdR :: BeerId -> Handler Value
getBeerByIdR beerId = do
    F.anyOriginIn [ F.OPTIONS, F.GET ]
    beer   <-  runDB $ get404 beerId
    sendStatusJSON ok200 $ object [ "resp" .= beer ]

deleteBeerByIdR :: BeerId -> Handler Value
deleteBeerByIdR beerId = do
    F.anyOriginIn [ F.OPTIONS, F.DELETE ]
    runDB $ delete beerId
    sendStatusJSON ok200 $ object ["resp" .= ("ok"::Text)]
    