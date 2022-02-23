#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.12
  --package scalpel
-}

{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Control.Monad
import Control.Applicative


main :: IO ()
main = do
  exampleHtml <- readFile "example.html"
  let scrapeResults = scrapeStringLike exampleHtml altTextAndImages
  printScrapeResults scrapeResults

printScrapeResults :: Maybe [(String,String,Maybe String)] -> IO ()
printScrapeResults Nothing = putStrLn "Something went wrong!"
printScrapeResults (Just []) = putStrLn "Couldn't scrape anything!"
printScrapeResults (Just results) = forM_ results print

altTextAndImages :: Scraper String [(String,String,Maybe String)]
altTextAndImages =
    chroot ("table" @: [hasClass "interesting"])
           (chroots "tr"
                    (do
                       firstCol <- text ("td" @: ["class" @= "first-column"])
                       secondCol <- text ("td" @: [notP ("class" @= "first-column")])
                       link <- optional $ attr "href" ("td" // "a")
                       return (strip firstCol, strip secondCol, link)))
    where
      strip = unwords . words