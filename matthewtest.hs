{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Control.Applicative
import Data.Text 


scrapper :: String -> IO (Maybe [Text])
scrapper url = scrapeURL url pricetag

pricetag :: Scraper Text [Text]
pricetag = texts $ "div" @: [hasClass "trackingProduct hidden"]


tokenizer :: [Text] -> [Text]
tokenizer = concatMap (concatMap processor . Text.words)

cleaner = Text.filter (`notElem` (",.><?!-:;" :: String))
processor = Text.splitOn "/" . Text.toLower . cleaner


mapReduce :: [Text] -> [(Text, Integer)]
mapReduce = reducer . mapper

mapper :: [Text] -> [(Text, Integer)]
mapper = map (, 1)

reducer :: [(Text, Integer)] -> [(Text, Integer)]
reducer = Map.toList . foldr (uncurry $ Map.insertWith (+)) Map.empty


sorted :: [(Text, Integer)] -> [(Text, Integer)]
sorted = sortBy $ \(_, x) (_, y) -> compare y x

pipe = sorted . mapReduce . tokenizer

sorted :: [(Text, Integer)] -> [(Text, Integer)]
sorted = sortBy $ \(_, x) (_, y) -> compare y x

main :: IO ()
main = do
result <- fmap pipe <$> scrapper url

case result of
  Nothing     -> putStrLn "Failed to scrap or parse"
  Just tokens -> mapM_ print $ filter byRelevancy tokens
    where
      byRelevancy :: (Text, Integer) -> Bool
      byRelevancy (token, count) = popular && relevant
        where
          popular  = count > 3
          relevant = token `elem` languages

languages :: [Text]
languages = [ "scala", "haskell", "clojure", "f#", "fsharp", "java", "erlang"
            , "javascript", "python", "scheme", "elixir", "lisp", "racket"
            , "elm", "purescript", "ml", "ocaml", "react", "reason", "reasonml"
            , "net", "jvm", "beam", "llvm", "scalajs", "clojurescript", "ghcjs"
            ]



--,.><?!-:;\´'()
--container-fluid
--body > div.container-fluid > div.panel.panel-main.productTop.panel-big.hideOverflow > div > div.trackingProduct.hidden > input[type=hidden]:nth-child(20)