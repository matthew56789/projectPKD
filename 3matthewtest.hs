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




scrapper :: String -> IO (Maybe [String])
scrapper url = scrapeURL url pricetag

pricetag :: Scraper String [String]
pricetag = texts $ "div" @: [hasClass "price-box"]
	--texts $ "div" @: [hasClass "text"] -- $ do
--	contents <- text anySelector
--	guard ("p" `isInfixOf` contents)
--	html anySelector




--    where do content <- hasClass anySelector
--            guard ("price" `isInfixOf` content)
--            html anySelector


--		contents <- anySelector
--		guard ("price" `isInfixOf` contents)
--		html anySelector


--texts $ "div" @: [hasClass "price"]


{--scrapper "https://www.mediamarkt.se/sv/product/_samsung-galaxy-s22-ultra-128gb-6-8-smartphone-black-1339393.html"


-}