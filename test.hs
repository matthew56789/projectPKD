import Network.HTTP.Conduit ( simpleHttp )
import Text.HTML.TagSoup ( (~/=), parseTags, innerText )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL

main :: IO ()
main = do
    lbs <- simpleHttp komplett
    let lastModifiedDateTime = fromFooter $ parseTags lbs
    
    putStrLn $ "Price: " ++ CL.unpack lastModifiedDateTime

    where fromFooter = CL.unwords . CL.words . innerText . take 2 . dropWhile (~/= "<span class=product-price-now>")
          komplett = "https://www.komplett.se/product/1184627/mobil-klockor/mobiltelefoner/iphone-12-64gb-lila"
