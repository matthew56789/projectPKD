{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Control.Monad
import Control.Applicative

pricetagKomplett :: Scraper String [String]
pricetagKomplett = texts $ "span" @: [hasClass "product-price-now"]

pricetagElgiganten :: Scraper String [String]
pricetagElgiganten = texts $ "div" @: [hasClass "feature__price"]

pricetagMediamarkt :: Scraper String [String]
pricetagMediamarkt = texts $ "div" @: [hasClass "price"]

pricetagNetonnet :: Scraper String [String]
pricetagNetonnet = texts $ "div" @: [hasClass "price-big"]



-- https://www.komplett.se/product/1181598/mobil-klockor/mobiltelefoner/oneplus-9-pro-8128gb-morning-mist?q=oneplus4

--tagen från labb 4
isPrefix mainstring substring
	| substring == "" = True
	| substring !! (length substring - 1) /= mainstring !! (length substring - 1) = False
	| substring !! (length substring - 1) == mainstring !! (length substring - 1) && 
	isPrefix mainstring (init substring) == True = True