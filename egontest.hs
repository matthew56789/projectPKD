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

fetchPrice :: String -> IO String
fetchPrice url
	| isPrefix url "https://www.komplett" = fetchPrice' url pricetagKomplett
	| isPrefix url "https://www.mediamarkt" = fetchPrice' url pricetagMediamarkt


fetchPrice' url scraper = do
	scraped <- scrapeURL url scraper
	let Just (x:xs) = scraped in
		return x

cleanInts "" = ""
cleanInts (x:xs) 
	| (x == '0') || (x == '1') || (x == '2') || (x == '3') || (x == '4') || (x == '5') || (x == '6') || (x == '7') || (x == '8') || (x == '9') = x : cleanInts xs
	| otherwise = cleanInts xs

priceCheck url = do
	result <- fetchPrice url
	return ("Price: " ++ (cleanInts result) ++ " kr")

--https://www.mediamarkt.se/sv/product/_oneplus-9-128-gb-6-55-smartphone-artic-sky-1333485.html
-- https://www.komplett.se/product/1181598/mobil-klockor/mobiltelefoner/oneplus-9-pro-8128gb-morning-mist?q=oneplus4

--tagen från labb 4
isPrefix mainstring substring
	| substring == "" = True
	| substring !! (length substring - 1) /= mainstring !! (length substring - 1) = False
	| substring !! (length substring - 1) == mainstring !! (length substring - 1) && 
	isPrefix mainstring (init substring) == True = True

-- removeJustIO :: Maybe a -> a
-- removeJustIO (Just x) = do
	-- return x