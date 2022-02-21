import Network.HTTP.Conduit ( simpleHttp )
import Text.HTML.TagSoup ( (~/=), parseTags, innerText )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL

main :: IO ()
main = do
    lbs <- simpleHttp "https://wiki.haskell.org"
    let lastModifiedDateTime = fromFooter $ parseTags lbs
    putStrLn $ "wiki.haskell.org was last modified on " 
        ++ CL.unpack lastModifiedDateTime
    where fromFooter = CL.unwords . drop 6 . CL.words
              . innerText . take 2 . dropWhile (~/= "<li id=footer-info-lastmod>")
