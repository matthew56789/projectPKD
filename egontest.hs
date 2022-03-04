{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel

{- priceCheck url
     checks the price of a product on a webpage
     PRE:  needs to be a compatible url
     RETURNS: returns the price of the product
     SIDE EFFECTS: ... side effects, if any, including exceptions ...
     EXAMPLES: priceCheck "https://cdon.se/hemelektronik/samsung-galaxy-s21-5g-sm-g991b-128gb-p-73382318?utm_source=prisjakt&utm_medium=cpc&utm_term=Samsung%20Galaxy%20S21%205G%20SM-G991B%20128GB&utm_content=73382318&utm_campaign=prisjakt_se_hemelektronik%20%3e%20mobiltelefoner%20%3e%20-tillbeh%c3%b6r%20%3e%20mobiltelefoner"
     == "Price: 6439 kr"
  -}
pricetagKomplett :: Scraper String [String]
pricetagKomplett = texts $ "span" @: [hasClass "product-price-now"]

pricetagMediamarkt :: Scraper String [String]
pricetagMediamarkt = texts $ "div" @: [hasClass "price"]

pricetagAmazon :: Scraper String [String]
pricetagAmazon = texts $ "span" @: [hasClass "a-price-whole"]

pricetagClasohlson :: Scraper String [String]
pricetagClasohlson = texts $ "span" @: [hasClass "product__price-value"]

pricetagCdonSearch :: Scraper String [String]
pricetagCdonSearch = texts $ "span" @: [hasClass "p-c__price-consumer"]

pricetagCdon :: Scraper String [String]
pricetagCdon = texts $ "span" @: ["id" @= "product-price"]

pricetagTelefon :: Scraper String [String]
pricetagTelefon = texts $ "span" @: [hasClass "price-amount"]--["itemprop" @= "price"]


{- fetchPrice url
     checks the price of a product on a webpage
     PRE:  needs to be a compatible url
     RETURNS: returns the price of the product
     SIDE EFFECTS: ... side effects, if any, including exceptions ...
     EXAMPLES: priceCheck "https://cdon.se/hemelektronik/samsung-galaxy-s21-5g-sm-g991b-128gb-p-73382318?utm_source=prisjakt&utm_medium=cpc&utm_term=Samsung%20Galaxy%20S21%205G%20SM-G991B%20128GB&utm_content=73382318&utm_campaign=prisjakt_se_hemelektronik%20%3e%20mobiltelefoner%20%3e%20-tillbeh%c3%b6r%20%3e%20mobiltelefoner"
     == "Price: 6439 kr"
  -}
fetchPrice :: String -> IO String
fetchPrice url
    | isPrefix url "https://www.komplett" = fetchPrice' url pricetagKomplett
    | isPrefix url "https://www.mediamarkt" = fetchPrice' url pricetagMediamarkt
    | isPrefix url "https://www.amazon" = fetchPrice' url pricetagAmazon
    | isPrefix url "https://www.clasohlson" = fetchPrice' url pricetagClasohlson
    | isPrefix url "https://cdon.se/catalog/search?q=" = fetchPrice' url pricetagCdonSearch
    | isPrefix url "https://cdon" = fetchPrice' url pricetagCdon
    | isPrefix url "https://www.telefonshoppen" = fetchPrice' url pricetagTelefon
    | otherwise  = error "Incompatible url"


{- priceCheck url
     checks the price of a product on a webpage
     PRE:  needs to be a compatible url
     RETURNS: returns the price of the product
     SIDE EFFECTS: ... side effects, if any, including exceptions ...
     EXAMPLES: priceCheck "https://cdon.se/hemelektronik/samsung-galaxy-s21-5g-sm-g991b-128gb-p-73382318?utm_source=prisjakt&utm_medium=cpc&utm_term=Samsung%20Galaxy%20S21%205G%20SM-G991B%20128GB&utm_content=73382318&utm_campaign=prisjakt_se_hemelektronik%20%3e%20mobiltelefoner%20%3e%20-tillbeh%c3%b6r%20%3e%20mobiltelefoner"
     == "Price: 6439 kr"
  -}
fetchPrice' :: String -> Scraper String [String] -> IO String
fetchPrice' url scraper = do
	scraped <- scrapeURL url scraper
	if scraped == Just [] 
		then error "no price found" 
			else
				let Just (x:xs) = scraped in
					return x


{- priceCheck url
     checks the price of a product on a webpage
     PRE:  needs to be a compatible url
     RETURNS: returns the price of the product
     SIDE EFFECTS: ... side effects, if any, including exceptions ...
     EXAMPLES: priceCheck "https://cdon.se/hemelektronik/samsung-galaxy-s21-5g-sm-g991b-128gb-p-73382318?utm_source=prisjakt&utm_medium=cpc&utm_term=Samsung%20Galaxy%20S21%205G%20SM-G991B%20128GB&utm_content=73382318&utm_campaign=prisjakt_se_hemelektronik%20%3e%20mobiltelefoner%20%3e%20-tillbeh%c3%b6r%20%3e%20mobiltelefoner"
     == "Price: 6439 kr"
  -}
cleanInts :: String -> String
cleanInts "" = ""
cleanInts (x:xs) 
	| (x == '0') || (x == '1') || (x == '2') || (x == '3') || (x == '4') || (x == '5') || (x == '6') || (x == '7') || (x == '8') || (x == '9') || (x == ',') = x : cleanInts xs
	| otherwise = cleanInts xs


 {- priceCheck url
     checks the price of a product on a webpage
     PRE:  needs to be a compatible url
     RETURNS: returns the price of the product
     SIDE EFFECTS: ... side effects, if any, including exceptions ...
     EXAMPLES: priceCheck "https://cdon.se/hemelektronik/samsung-galaxy-s21-5g-sm-g991b-128gb-p-73382318?utm_source=prisjakt&utm_medium=cpc&utm_term=Samsung%20Galaxy%20S21%205G%20SM-G991B%20128GB&utm_content=73382318&utm_campaign=prisjakt_se_hemelektronik%20%3e%20mobiltelefoner%20%3e%20-tillbeh%c3%b6r%20%3e%20mobiltelefoner"
     == "Price: 6439 kr"
  -}
priceCheck :: String -> IO String
priceCheck url = do
	result <- fetchPrice url
	return ("Price: " ++ (cleanInts result) ++ " kr")

--https://www.mediamarkt.se/sv/product/_oneplus-9-128-gb-6-55-smartphone-artic-sky-1333485.html
-- https://www.komplett.se/product/1181598/mobil-klockor/mobiltelefoner/oneplus-9-pro-8128gb-morning-mist?q=oneplus4

--tagen från labb 4

isPrefix :: String -> String -> Bool
isPrefix mainstring substring
	| (length substring) > (length mainstring) = False
	| substring == "" = True
	| substring !! (length substring - 1) /= mainstring !! (length substring - 1) = False
	| substring !! (length substring - 1) == mainstring !! (length substring - 1) && 
	isPrefix mainstring (init substring) == True = True

-- removeJustIO :: Maybe a -> a
-- removeJustIO (Just x) = do
	-- return x