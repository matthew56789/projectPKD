{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Control.Monad
import Control.Applicative
import Test.HUnit

pricetagKomplett :: Scraper String [String]
pricetagKomplett = texts $ "span" @: [hasClass "product-price-now"]

pricetagMediamarkt :: Scraper String [String]
pricetagMediamarkt = texts $ "div" @: [hasClass "price"]

pricetagAmazon :: Scraper String [String]
pricetagAmazon = texts $ "span" @: [hasClass "a-price-whole"]

pricetagCdon :: Scraper String [String]
pricetagCdon = texts $ "span" @: ["id" @= "product-price"]

pricetagElectronordic :: Scraper String [String]
pricetagElectronordic = texts $ "span" @: [hasClass "price"]


fetchPrice :: String -> IO String
fetchPrice url
    | isPrefix url "https://www.komplett" = fetchPrice' url pricetagKomplett
    | isPrefix url "https://www.mediamarkt" = fetchPrice' url pricetagMediamarkt
    | isPrefix url "https://www.amazon" = fetchPrice' url pricetagAmazon
    | isPrefix url "https://cdon" = fetchPrice' url pricetagCdon
	| isPrefix url "https://electronordic" = fetchPrice' url pricetagElectronordic
    | otherwise  = error "Incompatible url"


fetchPrice' url scraper = do
	scraped <- scrapeURL url scraper
	if scraped == Just [] then return "" else
		let Just (x:xs) = scraped in
			return x


cleanInts "" = ""
cleanInts (x:xs) 
	| (x == '0') || (x == '1') || (x == '2') || (x == '3') || (x == '4') || (x == '5') || (x == '6') || (x == '7') || (x == '8') || (x == '9') || (x == ',') || (x == '.') = x : cleanInts xs
	| otherwise = cleanInts xs


priceCheck :: String -> IO String
priceCheck url = do
	result <- fetchPrice url
	if result == "" then error "Could not find a price" else
		return ("Price: " ++ (cleanInts result) ++ " kr")


priceCompare str = do
	
	komplett <- fetchPrice ("https://www.komplett.se/search?q=" ++ str)
	putStrLn ("Komplett: " ++ (cleanInts komplett) ++ "kr")
	
	mediamarkt <- fetchPrice ("https://www.mediamarkt.se/sv/search.html?query=" ++ str)
	putStrLn ("MediaMarkt: " ++ (cleanInts mediamarkt) ++ "kr")
	
	amazon <- fetchPrice ("https://www.amazon.se/s?k=" ++ str)
	putStrLn ("Amazon: " ++ (cleanInts amazon) ++ "kr")
	
	electronordic <- fetchPrice ("https://electronordic.se/catalogsearch/result/?q=" ++ str)
	putStrLn ("ElectroNordic: " ++ (cleanInts electronordic) ++ "kr")
	
	cdon <- fetchPrice ("https://cdon.se/catalog/search?q=" ++ str)
	putStrLn ("CDON: " ++ (cleanInts mediamarkt) ++ "kr")


--tagen från labb 4
isPrefix mainstring substring
	| (length substring) > (length mainstring) = False
	| substring == "" = True
	| substring !! (length substring - 1) /= mainstring !! (length substring - 1) = False
	| substring !! (length substring - 1) == mainstring !! (length substring - 1) && 
	isPrefix mainstring (init substring) == True = True

-- price1 <- priceCheck "https://www.mediamarkt.se/sv/product/_oneplus-9-128-gb-6-55-smartphone-artic-sky-1333485.html"
-- testCase1 :: Test
-- testCase1 =	TestCase $ assertEqual "priceCheck" ("Price: 6690 kr") price1

-- test2 = TestCase (do (x,y) <- partA 3
	-- assertEqual "for the first result of partA," 5 x
	-- b <- partB y
	-- assertBool ("(partB " ++ show y ++ ") failed") b)
-- TestCase $ assertEqual "priceCheck" ("Price: 6690 kr") (priceCheck "https://www.mediamarkt.se/sv/product/_oneplus-9-128-gb-6-55-smartphone-artic-sky-1333485.html")

-- testCase1 = TestCase ((do 
	-- price <- priceCheck "https://www.mediamarkt.se/sv/product/_oneplus-9-128-gb-6-55-smartphone-artic-sky-1333485.html" 
	-- return price)
	-- assertEqual "priceCheck Mediamarkt" ("Price: 6690 kr") price)