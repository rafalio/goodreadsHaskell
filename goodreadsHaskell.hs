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

type DevKey = String
type UserId = String
type GoodreadsURL = String
type Page = Int

data Review = Review {
  rBook       :: Book,
  rRating     :: Int,
  rRead       :: Maybe UTCTime,
  rAdded      :: UTCTime,
  rReviewText :: String,
  rLink       :: String
} deriving (Eq,Ord,Show)

instance XmlPickler Review where
  xpickle = xpReview

data Book = Book {
  bTitle    :: String,
  bImageUrl :: String,
  bLink     :: String,
  bNumPages :: Maybe Int
} deriving (Eq,Ord,Show)

instance XmlPickler Book where
  xpickle = xpBook

data GRRequestConfig = GRRequestConfig {
  grDevKey :: DevKey,
  grUserId :: UserId
}

myConfig = GRRequestConfig {
  grDevKey = "qgmEyVxWe68iCu4TMGbKIw", -- Get your own
  grUserId = "6752954" -- your goodreads ID
}

main = do
  reviews <- reviewsUpToPage myConfig 5
  putStrLn $ "Number of reviews: " ++ show (length reviews)
  mapM_ (print . fmap (bTitle . rBook)) reviews

-- A list of your book reviews up to the specified page (20 per page)
reviewsUpToPage :: GRRequestConfig -> Page -> IO [Either String Review]
reviewsUpToPage config n = do
  responses <- downloadPagesData config 1 n
  let responseBodies = fmap (\r -> r ^. responseBody) responses
  return $ readXmlReviews $ concatMap parseGRResponse responseBodies

mkReadShelfURL :: UserId -> GoodreadsURL
mkReadShelfURL uid = grBaseURL ++ uid
  where grBaseURL = "https://www.goodreads.com/review/list/"

mkReadShelfRequestParams :: DevKey -> Page -> Options
mkReadShelfRequestParams key page = defaults & params .~ paramList
  where
    paramList = [("shelf","read"),
                  ("key",T.pack key),
                  ("v","2"),
                  ("format","xml"),
                  ("per_page","20"),  -- 20 is the goodreads limit
                  ("page", (T.pack . show $ page))]  

parseGRResponse rBody = parsed
  where
    isReview = hasName "review"
    bookTags = ["title", "image_url", "link", "num_pages"]
    reviewTags = ["book", "rating", "date_added", "read_at", "body", "link"]

    bookFilter = foldl1 (<+>) (fmap hasName bookTags)
    reviewFilter = foldl1 (<+>) (fmap hasName reviewTags)

    parsed = runLA (
        xreadDoc >>>
        deep isReview >>>
        transfAllCdata >>>
        processChildren reviewFilter >>> 
        processChildren (processChildren bookFilter `when` hasName "book")
        ) $ (LB.unpack rBody)

readXmlReviews :: [XmlTree] -> [Either String Review]
readXmlReviews = fmap readXmlReview

readXmlReview :: XmlTree -> Either String Review
readXmlReview = (unpickleDoc' xpickle)

downloadPagesData :: GRRequestConfig -> Page -> Page -> IO [Response LB.ByteString]
downloadPagesData conf from to = sequence actions
  where
    opts    = map (mkReadShelfRequestParams $ grDevKey conf) [from..to]
    url     = mkReadShelfURL (grUserId conf)
    actions = zipWith getWith opts (repeat url)


uncurry6 fx = \(a,b,c,d,e,f) -> fx a b c d e f

xpTime = xpWrapEither (timeParse, show) xpText
  where 
    timeParse = parseTimeM True defaultTimeLocale timeFormatString
    timeFormatString = "%a %b %d %X %z %Y"

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
