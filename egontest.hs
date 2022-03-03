{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Control.Monad
import Control.Applicative

pricetagKomplett :: Scraper String [String]
pricetagKomplett = texts $ "span" @: [hasClass "product-price-now"]

pricetagElgiganten :: Scraper String [String]
pricetagElgiganten = texts $ "span" @: [hasClass "price"]

pricetagMediamarkt :: Scraper String [String]
pricetagMediamarkt = texts $ "div" @: [hasClass "price"]

pricetagNetonnet :: Scraper String [String]
pricetagNetonnet = texts $ "div" @: [hasClass "price-big"]

pricetagAmazon :: Scraper String [String]
pricetagAmazon = texts $ "span" @: [hasClass "a-price-whole"]

pricetagInet :: Scraper String [String]
pricetagInet = texts $ "span" @: [hasClass "price"]

pricetagClasohlson :: Scraper String [String]
pricetagClasohlson = texts $ "span" @: [hasClass "product__price-value"]


fetchPrice :: String -> IO String
fetchPrice url
    | isPrefix url "https://www.komplett" = fetchPrice' url pricetagKomplett
    | isPrefix url "https://www.mediamarkt" = fetchPrice' url pricetagMediamarkt
    | isPrefix url "https://www.amazon" = fetchPrice' url pricetagAmazon
    | isPrefix url "https://www.clasohlson" = fetchPrice' url pricetagClasohlson
    | isPrefix url "https://www.inet" = fetchPrice' url pricetagInet
    | otherwise  = error "Incompatible url"

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
	| (length substring) > (length mainstring) = False
	| substring == "" = True
	| substring !! (length substring - 1) /= mainstring !! (length substring - 1) = False
	| substring !! (length substring - 1) == mainstring !! (length substring - 1) && 
	isPrefix mainstring (init substring) == True = True

-- removeJustIO :: Maybe a -> a
-- removeJustIO (Just x) = do
	-- return x