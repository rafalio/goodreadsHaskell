{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wreq
import Control.Lens hiding (deep, Review)

import Text.XML.HXT.Core
import Text.XML.HXT.DOM.FormatXmlTree
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Arrow.Edit
import Text.XML.HXT.Arrow.Pickle.Schema

import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Text as T


data Review = Review {
  rBook       :: Book,
  rRating     :: Int,
  rRead       :: Maybe UTCTime,
  rAdded      :: UTCTime,
  rReviewText :: String,
  rLink       :: String
} deriving (Eq,Ord,Show)

data Book = Book {
  bTitle    :: String,
  bImageUrl :: String,
  bLink     :: String,
  bNumPages :: Maybe Int
} deriving (Eq,Ord,Show)


uncurry6 fx = \(a,b,c,d,e,f) -> fx a b c d e f

xpTime = xpWrapEither (timeParse, show) xpText
  where 
    timeParse = parseTimeM True defaultTimeLocale timeFormatString
    timeFormatString = "%a %b %d %X %z %Y"

instance XmlPickler Review where
  xpickle = xpReview

instance XmlPickler Book where
  xpickle = xpBook

xpReview = xpElem "review" $
  xpWrap (uncurry6 Review, \r -> (rBook r, rRating r, rRead r, rAdded r, rReviewText r, rLink r)) $
  xp6Tuple
    xpBook
    (xpElem "rating" $ xpInt)
    (xpElem "read_at" $ xpOption xpTime)
    (xpElem "date_added" $ xpTime)
    (xpElem "body" $ xpText)
    (xpElem "link" $ xpText)

xpBook = xpElem "book" $
      xpWrap (uncurry4 Book, \b -> (bTitle b, bImageUrl b, bLink b, bNumPages b)) $
      xp4Tuple
        (xpElem "title" $ xpText)
        (xpElem "image_url" $ xpText)
        (xpElem "link" $ xpText)
        (xpElem "num_pages" $ xpOption xpInt)



type DevKey = String
type UserId = String
type GoodreadsURL = String
type Page = Int

devKey = "qgmEyVxWe68iCu4TMGbKIw"
myId = "6752954"

mkReadShelfURL :: UserId -> GoodreadsURL
mkReadShelfURL uid = grBaseURL ++ uid
  where grBaseURL = "https://www.goodreads.com/review/list/"


mkReadShelfRequestParams :: Page -> Options
mkReadShelfRequestParams page = defaults & params .~ paramList
  where
    paramList = [("shelf","read"),
                  ("key",devKey),
                  ("v","2"),
                  ("format","xml"),
                  ("per_page","200"),
                  ("page", (T.pack . show $ page))]  
    

parseGRResponse rBody = parsed
  where
    isReview = hasName "review"
    bookTags = ["title", "image_url", "link", "num_pages"]
    reviewTags = ["book", "rating", "date_added", "read_at", "body", "link"]

    bookFilter = foldl1 (<+>) (fmap hasName bookTags)
    reviewFilter = foldl1 (<+>) (fmap hasName reviewTags)

    filter = isReview

    parsed = runLA (
        xreadDoc >>> 
        deep filter >>>
        transfAllCdata >>>
        processChildren reviewFilter >>> 
        processChildren (processChildren bookFilter `when` hasName "book")
        ) $ (LB.unpack rBody)

xmlTreesToHaskell ts = map (unpickleDoc' xpickle) ts :: [Either String Review]

main = do
  let opts = mkReadShelfRequestParams 2
  r <- getWith opts (mkReadShelfURL myId)
  let rBody = r ^. responseBody 
  let parsed = parseGRResponse rBody
  putStrLn $ "Number of trees parsed: " ++ show (length parsed)
  let unpacked = xmlTreesToHaskell parsed
  let pretty = fmap formatXmlTree parsed
  mapM_ (print . fmap (bTitle . rBook)) unpacked

{-

Xtag "GoodreadsResponse"
  XTag "reviews"
    XTag "review"
      XTag "book"
        XTag "title"
        XTag "image_url"
        XTag "link"
        XTag "num_pages"
      XTag "rating"
      XTag "date_added"
      XTag "read_at"
      XTag "body" // the body of the text that I wrote
      XTag "url"
-} 
