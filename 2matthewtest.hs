{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Control.Monad
import Control.Applicative
-- import Network.HTTP.Conduit ( simpleHttp )
-- import qualified Data.ByteString.Lazy.Char8 as CL

main :: IO ()
main = do
  -- testhtml <- simpleHttp "https://www.elgiganten.se/product/mobiler-tablets-smartklockor/mobiltelefon/oneplus-8-pro-smartphone-8128gb-onyx-black/162403"
  -- let exampleHtml = CL.unpack testhtml
  exampleHtml <- readFile "Elgiganten.html"
  let scrapeResults = scrapeStringLike exampleHtml altTextAndImages
  printScrapeResults scrapeResults

printScrapeResults Nothing = putStrLn "Something went wrong!"
printScrapeResults (Just []) = putStrLn "Couldn't scrape anything!"
printScrapeResults (Just results) = forM_ results print

altTextAndImages :: Scraper String [String]
altTextAndImages = texts $ "div" @: [hasClass "feature__price"]
   -- chroots ("div" @: [hasClass "feature__price ng-star-inserted"]) (text "")

-- main :: IO ()
-- main = do 
	-- testhtml <- simpleHttp "https://www.example.com/"
	-- let exampleHtml = CL.unpack testhtml
	-- putStrLn exampleHtml