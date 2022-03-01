{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Control.Monad
import Control.Applicative
import Data.Text 

{-}
scrapper :: String -> IO (Maybe [String])
scrapper url = scrapeURL url pricetag


pricetag :: Scraper String [String]
pricetag = texts $ "div" @: [hasClass "feature__price"]
-}

main :: IO ()
main = do
  url <- getLine
  let scrapeResults = scrapeStringLike (scrapeURL url) altTextAndImages
  printScrapeResults scrapeResults

printScrapeResults Nothing = putStrLn "Something went wrong!"
printScrapeResults (Just []) = putStrLn "Couldn't scrape anything!"
printScrapeResults (Just results) = forM_ results print

altTextAndImages :: Scraper String [String]
altTextAndImages = texts $ "div" @: [hasClass "feature__price"]
   -- chroots ("div" @: [hasClass "feature__price ng-star-inserted"]) (text "")

--"https://www.elgiganten.se/product/mobiler-tablets-smartklockor/mobiltelefon/samsung-galaxy-s22-ultra-5g-smartphone-12256gb-phantom-white/414929"