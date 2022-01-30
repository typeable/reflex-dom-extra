-- Pagination panel widget.
module Reflex.Dom.Pagination where

import Control.Monad (join)
import qualified Data.Text as T
import Reflex.Dom

type PageNumber = Int

-- | Pagination configuration.
data PaginationConfig t m = PaginationConfig
  { pagesNumber :: PageNumber
  -- ^ Total number of pages.
  , initPage :: PageNumber
  -- ^ Initial page number.
  , pageNumberElem
    :: Dynamic t PageNumber
    -> PageNumber
    -> m (Element EventResult (DomBuilderSpace m) t)
  -- ^ Element to display every page number given current page.
  , nextPageElem
    :: Dynamic t PageNumber
    -> m (Element EventResult (DomBuilderSpace m) t)
  -- ^ "Next page" element.
  , prevPageElem
    :: Dynamic t PageNumber
    -> m (Element EventResult (DomBuilderSpace m) t)
  -- ^ "Previous page" element.
  , ellipsisWidget :: m ()
  -- ^ Ellipsis between pages.
  , pageUpdateEvent :: Event t PageNumber
  -- ^ Event to update page number.
  , pageNumberSegmentSize :: Int
  -- ^ Number of pages after first page, before and after selected page
  -- and before last page. Skipped pages are replaced with ellipsis.
  , hideWhenEmpty :: Bool
  -- ^ Hide pagination panel when there are zero total pages?
  }

-- | Sanity check for config.
checkConfig :: PaginationConfig t m -> Bool
checkConfig PaginationConfig{..} =
  pagesNumber >= 0 &&
  initPage > 0 &&
  pageNumberSegmentSize >= 0

-- | Default element to display page number (current page is darker).
defaultPageNumberWidget
  :: MonadWidget t m
  => Dynamic t PageNumber
  -> PageNumber
  -> m (Element EventResult (DomBuilderSpace m) t)
defaultPageNumberWidget curDyn pageNum = fmap fst . elDynAttr' "button"
  (mkAttrs <$> curDyn) . text . show' $ pageNum
  where
    show' = T.pack . show
    mkAttrs cur = "type" =: "button" <>
      if pageNum == cur
        then "style" =: "background-color: #CCCCCC;"
        else mempty

-- | Default element for previous page (button with text "Prev").
defaultPrevPage
  :: MonadWidget t m
  => Dynamic t PageNumber
  -> m (Element EventResult (DomBuilderSpace m) t)
defaultPrevPage curDyn = fmap fst $
  elDynAttr' "button" (mkAttrs <$> curDyn) $ text "Prev"
  where
    mkAttrs cur = "type" =: "button" <>
      if cur == 1
        then "disabled" =: "" <> "style" =: "pointer-events:none;"
        else mempty

-- | Default element for next page (button with text "Next").
defaultNextPage
  :: MonadWidget t m
  => PageNumber
  -> Dynamic t PageNumber
  -> m (Element EventResult (DomBuilderSpace m) t)
defaultNextPage total curDyn = fmap fst $
  elDynAttr' "button" (mkAttrs <$> curDyn) $ text "Next"
  where
    mkAttrs cur = "type" =: "button" <>
      if cur == total
        then "disabled" =: "" <> "style" =: "pointer-events:none;"
        else mempty

-- | Default ellipsis.
defaultEllipsis
  :: MonadWidget t m
  => m ()
defaultEllipsis = text "..."

-- | Default config based on total number of items and page size.
defaultPaginationConfig
  :: MonadWidget t m
  => Int
  -> Int
  -> PaginationConfig t m
defaultPaginationConfig totalItems pageSize = PaginationConfig
  { pagesNumber = totalPages
  , initPage = 1
  , pageNumberElem = defaultPageNumberWidget
  , nextPageElem = defaultNextPage totalPages
  , prevPageElem = defaultPrevPage
  , ellipsisWidget = defaultEllipsis
  , pageUpdateEvent = never
  , pageNumberSegmentSize = 1
  , hideWhenEmpty = True }
  where
    totalPages = calcPages totalItems pageSize

-- | Calculate total number of pages given input length and page size.
calcPages :: Int -> Int -> Int
calcPages totalSize pageSize = (totalSize + pageSize - 1) `div` pageSize

-- | Pagination widget that takes dynamic config (useful when total number
-- of items may change dynamically).
paginationDyn
  :: MonadWidget t m
  => Dynamic t (PaginationConfig t m)
  -> m (Dynamic t PageNumber)
paginationDyn dynCfg = do
  pageEvDyn <- dyn $ pagination <$> dynCfg
  pageDynDyn <- holdDyn (initPage <$> dynCfg) pageEvDyn
  return (join pageDynDyn)

-- | Pagination widget that renders page numbers and returns current page
-- number.
pagination
  :: MonadWidget t m
  => PaginationConfig t m
  -> m (Dynamic t PageNumber)
pagination cfg@PaginationConfig{..}
  | not (checkConfig cfg) = return (constDyn initPage)
  | pagesNumber == 0 && hideWhenEmpty = return (constDyn initPage)
  | otherwise = mdo
    let shownPagesDyn = mkShownPages <$> currentPage
    prevEl <- prevPageElem currentPage
    pageSwitchEe <- (fmap leftmost) <$>
      (dyn $ mapM (pageSelector currentPage) <$> shownPagesDyn)
    pageSelectEvent <- switchHold never pageSwitchEe
    nextEl <- nextPageElem currentPage
    let
      prevPageEvent = domEvent Click prevEl
      nextPageEvent = domEvent Click nextEl
    currentPage <- foldDyn ($) initPage $ leftmost
      [ decPage <$ prevPageEvent
      , setPage <$> pageSelectEvent
      , incPage <$ nextPageEvent
      , const <$> pageUpdateEvent ]
    return currentPage
    where
      decPage n = max 1 (n-1)
      incPage n = min (n+1) pagesNumber
      setPage newPage currentPage = if newPage `elem` [1 .. pagesNumber]
        then newPage
        else currentPage
      pageSelector _ Nothing = ellipsisWidget >> return never
      pageSelector cur (Just n) = do
        selElem <- pageNumberElem cur n
        return (n <$ domEvent Click selElem)
      mkShownPages cur = fixPages $
        [1 .. min (1+pageNumberSegmentSize) pagesNumber] ++
        [max 1 (cur-pageNumberSegmentSize) ..
          min (cur+pageNumberSegmentSize) pagesNumber] ++
        [max 1 (pagesNumber-pageNumberSegmentSize) .. pagesNumber]
      fixPages (x : y : rest)
        | y <= x = fixPages (x : rest)
        | x + 1 < y = Just x : Nothing : fixPages (y : rest)
        | otherwise = Just x : fixPages (y :rest)
      fixPages xs = map Just xs
