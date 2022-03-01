module Main where

import Network.HTTP ( getRequest, getResponseBody, simpleHTTP )

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

main :: IO ()
main = do
    src <- openURL "http://wiki.haskell.org/Haskell"
    writeFile "temp.html" src
