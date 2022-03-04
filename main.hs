{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------------

import Text.HTML.Scalpel ( scrapeURL, texts, (@:), (@=), hasClass, Scraper, URL )
import Control.Monad ()
import Control.Applicative ()
import Test.HUnit ()


pricetagKomplett :: Scraper String [String]
pricetagKomplett = texts $ "span" @: [hasClass "product-price-now"]

pricetagMediamarkt :: Scraper String [String]
pricetagMediamarkt = texts $ "div" @: [hasClass "price"]

pricetagAmazon :: Scraper String [String]
pricetagAmazon = texts $ "span" @: [hasClass "a-price-whole"]

pricetagCdon :: Scraper String [String]
pricetagCdon = texts $ "span" @: ["id" @= "product-price"]

pricetagCdonSearch :: Scraper String [String]
pricetagCdonSearch = texts $ "span" @: [hasClass "p-c__price-consumer"]

pricetagElectronordic :: Scraper String [String]
pricetagElectronordic = texts $ "span" @: [hasClass "price"]



{-
	fetchPrice url
	
	Return: IO String
	Example:    fetchPrice "https://www.mediamarkt.se/sv/product/_oneplus-9-128-gb-6-55-smartphone-artic-sky-1333485.html"
					= "6690:-"
-}
fetchPrice :: URL -> IO String
fetchPrice url
    | isPrefix url "https://www.komplett" = fetchPrice' url pricetagKomplett
    | isPrefix url "https://www.mediamarkt" = fetchPrice' url pricetagMediamarkt
    | isPrefix url "https://www.amazon" = fetchPrice' url pricetagAmazon
	| isPrefix url "https://cdon.se/catalog/search?q=" = fetchPrice' url pricetagCdonSearch
    | isPrefix url "https://cdon" = fetchPrice' url pricetagCdon
	| isPrefix url "https://electronordic" = fetchPrice' url pricetagElectronordic
    | otherwise  = error "Incompatible url"



{-
	fetchPrice' url scraper

	Return: IO String
	Example:    fetchPrice' "https://www.mediamarkt.se/sv/product/_oneplus-9-128-gb-6-55-smartphone-artic-sky-1333485.html" pricetagMediamarkt
					= "6690:-"
-}
fetchPrice' :: URL -> Scraper String [String] -> IO String
fetchPrice' url scraper = do
        scraped <- scrapeURL url scraper
        if scraped == Just [] then return "" else
                let Just (x:xs) = scraped in
                        return x


{-
	cleanInts str
	Delete all characters that are not [1 ... 9], (.) or (,) from a list of characters.
	Return: a list of characters
	Example:    cleanInts "12345/w/r/33" = "1234533"
-}
cleanInts :: [Char] -> [Char]
cleanInts "" = ""
cleanInts (x:xs)
	| (x == '0') || (x == '1') || (x == '2') || (x == '3') || (x == '4') || (x == '5') || (x == '6') || (x == '7') || (x == '8') || (x == '9') = x : cleanInts xs
	| (x == ',') || (x == '.') = ""
	| otherwise = cleanInts xs



{-
	priceCheck url
	Find the price from an URL link
	Return: IO String
	Example:    priceCheck "https://www.mediamarkt.se/sv/product/_oneplus-9-128-gb-6-55-smartphone-artic-sky-1333485.html"
					= "Price: 6690 kr"
-}
priceCheck :: URL -> IO String
priceCheck url = do
        result <- fetchPrice url
        if result == "" then error "Could not find a price" else
                return ("Price: " ++ cleanInts result ++ " kr")




{-
	priceCompare productName
	Search for the price of a product 
	Return: IO ()
	Example:    priceCompare "iPhone 12 64GB Svart" = 	Komplett: 7990kr
														MediaMarkt: 7990kr
														Amazon: 7490,kr
														ElectroNordic: 7989,00kr
														CDON: 7990kr
-}
priceCompare :: [Char] -> IO ()
priceCompare str = let
	stringOfPrice "" = "No product found"
	stringOfPrice price = (cleanInts price) ++ " kr" in do

	komplett <- fetchPrice ("https://www.komplett.se/search?q=" ++ str)
	putStrLn ("Komplett: " ++ (stringOfPrice komplett))
	
	mediamarkt <- fetchPrice ("https://www.mediamarkt.se/sv/search.html?query=" ++ str)
	putStrLn ("MediaMarkt: " ++ (stringOfPrice mediamarkt))
	
	amazon <- fetchPrice ("https://www.amazon.se/s?k=" ++ str)
	putStrLn ("Amazon: " ++ (stringOfPrice amazon))
	
	electronordic <- fetchPrice ("https://electronordic.se/catalogsearch/result/?q=" ++ str)
	putStrLn ("ElectroNordic: " ++ (stringOfPrice electronordic))
	
	cdon <- fetchPrice ("https://cdon.se/catalog/search?q=" ++ str)
	putStrLn ("CDON: " ++ (stringOfPrice cdon))
	
	let 
		readCleanList [] = []
		readCleanList (x:xs)
			| x == "" = 99999999 : readCleanList xs
			| otherwise = (read (cleanInts x)::Int) : readCleanList xs
		
		nameList = ["Komplett", "MediaMarkt", "Amazon", "ElectroNordic", "CDON"]
		
		compList = readCleanList [komplett, mediamarkt, amazon, electronordic, cdon]
		
		minEle = elementIndex compList (minimum compList) 0
		in
		
		if (compList !! minEle) /= 99999999 then do
			putStrLn " "
			putStrLn ("Lowest price: " ++ nameList !! minEle ++ ": " ++ (show (compList !! minEle)) ++ " kr") 
		
		else do
			putStrLn " "
			putStrLn "Error: Product could not be found"
	






-- returns the index of the first element in the list equal to ele. If no such element exists, returns -1
-- uses an accumulator acc, which should be 0 initially
elementIndex :: (Eq a, Num b) => [a] -> a -> b -> b
elementIndex [] ele acc = -1
elementIndex (x:xs) ele acc
	| x == ele = acc
	| otherwise = elementIndex xs ele (acc + 1)




{-
	isPrefix mainString subString
	Check if the mainString is a prefix of subString
	Return: Bool
	Example: 	isPrefix "Hallo World" "Hallo" = True
	
	variants: The length of subString
-}
isPrefix :: String -> String -> Bool
isPrefix mainString subString
        | length subString > length mainString = False
        | subString == "" = True
        | subString !! (length subString - 1) /= mainString !! (length subString - 1) = False
        | subString !! (length subString - 1) == mainString !! (length subString - 1) &&
        isPrefix mainString (init subString) = True
        | otherwise = False






-------------------------------------

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