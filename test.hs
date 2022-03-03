import Network.HTTP.Conduit ( simpleHttp )
import Text.HTML.TagSoup ( (~/=), parseTags, innerText )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL


-- main :: IO ()
-- main = do
    -- lbs <- simpleHttp "https://wiki.haskell.org"
    -- let lastModifiedDateTime = fromFooter $ parseTags lbs
    -- putStrLn $ "wiki.haskell.org was last modified on " 
        -- ++ CL.unpack lastModifiedDateTime
    -- where fromFooter = CL.unwords . drop 6 . CL.words
              -- . innerText . take 2 . dropWhile (~/= "<li id=footer-info-lastmod>")

printHtml :: String -> IO String
printHtml x = do
	htp <- simpleHttp x
	return (CL.unpack htp)

getJScript :: String -> IO String
getJScript x = do
    htp <- simpleHttp x
    let jScripts = scriptFilter $ parseTags htp
    return (CL.unpack jScripts)
    where scriptFilter = CL.unwords . CL.words
              . innerText . dropWhile (~/= "<script>")

fetchPrice url = do
	scripts <- getJScript url
	return (take 20 (drop (searchString scripts "price") scripts))

-- extractInts (x:y:xs)
	-- | x 

--dragen rakt från min labb 5
searchString mainstring substring = searchStringAux mainstring substring 0

searchStringAux mainstring substring position
	| length substring > length mainstring = (-1)
	| substring == mainstring = position
searchStringAux mainstring substring position = 
	if take (length substring) mainstring == substring then position
	else searchStringAux (tail mainstring) substring (position + 1)

-- httpsimple = do
	-- htp <- simpleHttp "https://www.komplett.se/product/1181598/mobil-klockor/mobiltelefoner/oneplus-9-pro-8128gb-morning-mist?q=oneplus"
	-- return (CL.unwords . CL.words . innerText . dropWhile 

-- main2 = let
	-- fromFooter = (CL.unwords . CL.words . innerText . dropWhile (~/= "<script>"))
	-- lastModifiedDateTime = fromFooter $ parseTags httpsimple in
	-- return (CL.unpack lastModifiedDateTime)


main :: IO ()
main = do
    lbs <- simpleHttp dustin
    let lastModifiedDateTime = fromFooter $ parseTags lbs
    
    putStrLn $ "Price: " ++ CL.unpack lastModifiedDateTime

    where fromFooter = CL.unwords . CL.words . innerText . take 2 . dropWhile (~/= "<span class=c-price>")
          komplett = "https://www.komplett.se/product/1184627/mobil-klockor/mobiltelefoner/iphone-12-64gb-lila"
          dustin = "https://www.dustinhome.se/product/5011275245/galaxy-s22-ultra"

