{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------------

import Text.HTML.Scalpel
import Test.HUnit

----------------------------------------------------------------

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
	Gives a (unpruned) price provided a link to a product on a supported website.
	PRE: Link must be of one of the supported sites.
	RETURNS: IO String, String being the price of the product link provided
	EXAMPLES:    fetchPrice "https://www.mediamarkt.se/sv/product/_oneplus-9-128-gb-6-55-smartphone-artic-sky-1333485.html"
					== "6690:-"
				fetchPrice "https://www.komplett.se/product/1181594/mobil-klockor/mobiltelefoner/oneplus-9-pro-8128gb-pine-green"
					== "9\160\&690:-"
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
	Helper function of fetchPrice. Provided a Scraper String [String] and a URL, returns
	the first result of the scraping on the URL. 
	RETURNS: IO String. String being the first result of the scraping, or "" if nothing was found.
	EXAMPLES:    fetchPrice' "https://www.mediamarkt.se/sv/product/_oneplus-9-128-gb-6-55-smartphone-artic-sky-1333485.html" pricetagMediamarkt
					= "6690:-"
-}
fetchPrice' :: URL -> Scraper String [String] -> IO String
fetchPrice' url scraper = do
        scraped <- scrapeURL url scraper
        if scraped == Just [] then return "" else
                let Just (x:xs) = scraped in
                        return x



{-
	priceCheck url
	Finds the price of a product from an URL link of one of the supported sites.
	PRE: The provided URL leads to a product from one of the sites
	RETURNS: IO String, String being "Price: <price> kr"
	EXAMPLES:    priceCheck "https://www.mediamarkt.se/sv/product/_oneplus-9-128-gb-6-55-smartphone-artic-sky-1333485.html"
					== "Price: 6690 kr"
-}
priceCheck :: URL -> IO String
priceCheck url = do
        result <- fetchPrice url
        if result == "" then error "No product price found" else
                return ("Price: " ++ cleanIntString result ++ " kr")



{-
	priceCompare productName
	Searches all supported sites for the product named and takes the first result of each site,
	(Komplett.se, MediaMarkt.se Amazon.se, ElectroNordic.se, CDON.com), as well as derives the lowest price
	RETURNS: IO ()
	SIDE EFFECTS: Prints the results of the search + comparison on the screen
	EXAMPLES:    priceCompare "iPhone 12 64GB Svart" = 	Komplett: 7990 kr
														MediaMarkt: 7990 kr
														Amazon: 7490 kr
														ElectroNordic: 7989 kr
														CDON: 7950 kr
														
														Lowest Price: Amazon: 7490 kr
-}
priceCompare :: String -> IO ()
priceCompare str = let

	--locally defined function - stringOfPrice
	--truncates and prunes the string of Int chars str, and puts " kr" after it.
	--in case of empty string returns "No product found"
	stringOfPrice "" = "No product found"
	stringOfPrice price = (cleanIntString price) ++ " kr" in do

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
	--locally defined function: readCleanList	
	{- readCleanList ls
	Removes all non-int chars on all strings in a list, and turns them into Ints. (also truncates them) 
	PRE: Strings in list must contain an Int char or be empty
	RETURNS: A list of ints. Empty strings become 9999999
	EXAMPLES: readCleanList ["1s50.99", ""] == [150, 9999999]
	-}
		readCleanList :: [String] -> [Int]
		readCleanList [] = []
		readCleanList (x:xs)
			| x == "" = 99999999 : readCleanList xs
			| otherwise = (read (cleanIntString x)::Int) : readCleanList xs
		
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



{-
	cleanIntString str
	Delete all characters that are not [1 .. 9] and any character after (.) or (,) from a list of characters.
	RETURNS: a list of characters (a string)
	VARIANT: length str
	EXAMPLES:    cleanIntString "12345/w/r/33" = "1234533"
				cleanIntString "499.99" = "499"
-}
cleanIntString :: [Char] -> [Char]
cleanIntString "" = ""
cleanIntString (x:xs)
	| (x == '0') || (x == '1') || (x == '2') || (x == '3') || (x == '4') || (x == '5') || (x == '6') || (x == '7') || (x == '8') || (x == '9') = x : cleanIntString xs
	| (x == ',') || (x == '.') = ""
	| otherwise = cleanIntString xs



  {- elementIndex ls ele acc
     Searches a list ls for the first element equal to ele, and returns the index
	 of it using an accumulator acc which is initially 0. (note that index starts at 0)
     PRE:  Function is initially called with acc = 0.
     RETURNS: an Int, being the index for the first ele in the list, or -1 if none exists.
	 VARIANT: length ls
     EXAMPLES: elementIndex [1,2,3,4,5,6] 4 0 == 3
			   elementIndex [1,2,3,4,5,6] 12 0 == -1
  -}
elementIndex :: (Eq a) => [a] -> a -> Int -> Int
elementIndex [] ele acc = -1
elementIndex (x:xs) ele acc
	| x == ele = acc
	| otherwise = elementIndex xs ele (acc + 1)




{-
	isPrefix mainString subString
	Checks if the mainString is a prefix of subString.
	RETURNS: True/False
	VARIANT: length subString
	EXAMPLES: 	isPrefix "Hallo World" "Hallo" == True
				isPrefix "Hallo World" "Hullo" == False
				isPrefix "Hallo World" "World" == False
	-}
isPrefix :: String -> String -> Bool
isPrefix mainString subString
        | length subString > length mainString = False
        | subString == "" = True
        | subString !! (length subString - 1) /= mainString !! (length subString - 1) = False
        | subString !! (length subString - 1) == mainString !! (length subString - 1) &&
        isPrefix mainString (init subString) = True
        | otherwise = False
--taken from lab 4.



-------------------------------------

test1 :: Test
test1 = TestCase (assertEqual "isPrefix str1 str2" True (isPrefix str1 str2))
	where str1 = "hejhej"
	      str2 = "hej"

test2 :: Test
test2 = TestCase (assertEqual "cleanIntString lst" "123" (cleanIntString str))
	where str = "123Hej"

test3 :: Test
test3 = TestCase (assertEqual "elementIndex lst 4 0" 3 (elementIndex lst 4 0))
	where lst = [1,2,3,4,5,6]



runtests :: IO Counts
runtests = runTestTT $ TestList [test1, test2, test3]
		  
--
--instance Eq a => Eq (IO a)
--instance Show a => Show (IO a)
--
--
--test2 :: Test
--test2 = TestCase (assertEqual "priceCheck str1" "Price: 4490 kr" (priceCheck str1))
--		where 	str1 = "https://www.mediamarkt.se/sv/product/_samsung-galaxy-s20-fe-4g-128gb-6gb-ram-6-5-smartphone-cloud-navy-1334227.html?utm_source=google&utm_medium=cpc&utm_campaign=bb-shopping-generic&utm_term=&utm_content=1334227&gclid=CjwKCAiAjoeRBhAJEiwAYY3nDIEywV0TAQUhASisPit9Pjwi1bJPTBrbln0pCHzkBDeHxdzR12FVuhoC2pYQAvD_BwE"
--
--runtests :: IO Counts
--runtests = runTestTT $ TestList [test1]