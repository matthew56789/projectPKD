{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Control.Monad
import Control.Applicative


main :: IO ()
main = do
  exampleHtml <- readFile "elgiganten.HTML"
  let scrapeResults = scrapeStringLike exampleHtml altTextAndImages
  printScrapeResults scrapeResults

printScrapeResults Nothing = putStrLn "Something went wrong!"
printScrapeResults (Just []) = putStrLn "Couldn't scrape anything!"
printScrapeResults (Just results) = forM_ results print

altTextAndImages :: Scraper String [String]
altTextAndImages = texts $ "div" @: [hasClass "feature__price"]
   -- chroots ("div" @: [hasClass "feature__price ng-star-inserted"]) (text "")