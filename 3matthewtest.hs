{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Control.Monad
import Control.Applicative
--import Data.Text 
import Data.List (isInfixOf)

{-
main = do
	link <- getLine
	let result = (scrapper url)
	printResult result

printScrapeResults Nothing = putStrLn "Something went wrong!"
printScrapeResults (Just []) = putStrLn "Couldn't scrape anything!"
printScrapeResults (Just results) = forM_ results print
-}



scrapper :: String -> IO (Maybe [[String]])
scrapper url = scrapeURL url pricetag


pricetag :: Scraper String [[String]]
pricetag = chroots (AnyTag anySelector) scrapeLink

scrapeLink :: Scraper String [String]
scrapeLink = fmap (map ("http://www.google.com/" ++)) (attrs "a" "href")

	--chroots ("input" @: ["name" @= "ProductPrice", "type" @= "hidden"]) (attr "value" "=")
	--chroots ("span" @: [hasClass "c-price"]) (attr "span" )
--texts $ "div" @: ["id" @= "tradein-price"]
--	chroots ("div" @: ["id" @= "tradein-price"]) (attr "data-totalprice")
--	chroots (AnyTag @: [hasClass "price-big", hasClass "text"]) $ do
--	contents <- text anySelector
--	guard ("2" `isInfixOf` contents)
--	html anySelector

--



--    where do content <- hasClass anySelector
--            guard ("price" `isInfixOf` content)
--            html anySelector

--		contents <- text anySelector
--		guard ("cat" `isInfixOf` contents)
--		html anySelector


--texts $ "div" @: [hasClass "price"]


{--scrapper "https://www.mediamarkt.se/sv/product/_samsung-galaxy-s22-ultra-128gb-6-8-smartphone-black-1339393.html"
price
scrapper "https://www.komplett.se/product/1196420/mobil-klockor/mobiltelefoner/iphone-13-128gb-blue"
product-price-now        span
scrapper "https://www.newegg.com/cyberpowerpc-gamer-master-gm60900/p/N82E16883230646?Item=N82E16883230646&cm_sp=Homepage_SS-_-P2_83-230-646-_-03012022"
li     price-current
scrapper "https://www.netonnet.se/art/mobil-smartwatch/mobiltelefoner/samsung-mobil/samsung-galaxy-a32-5g-64gb-black/1016614.9050/"
div     price-big    funkar ej
-}