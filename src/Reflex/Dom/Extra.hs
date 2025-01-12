-- Common helpers to build DOM with Reflex.
module Reflex.Dom.Extra where

import Control.Lens
import Control.Monad.Fix
import Data.Bitraversable
import Data.Char
import Data.Either
import Data.Map as M
import Data.Maybe
import Data.Text as T hiding (zip, map)
import Data.Zip as Z
-- import Language.Javascript.JSaddle
import Prelude hiding (zip, unzip, zipWith)
import Reflex.Dom
-- import JavaScript.Object.Internal as JS

-- | @<meta>@ element.
meta :: DomBuilder t m => Map Text Text -> m ()
meta attrs = elAttr "meta" attrs blank

-- | Stylesheet link.
stylesheet :: DomBuilder t m => Text -> m ()
stylesheet ref = elAttr "link" ("rel" =: "stylesheet" <> "href" =: ref) blank

-- | <script> tag. Do not use in dynamic contexts, or it will be executed twice.
script :: DomBuilder t m => Text -> m ()
script url = elAttr "script" ("type" =: "text/javascript" <> "src" =: url) blank

-- | @<main>@ element with given CSS class.
mainClass :: DomBuilder t m => Text -> m a -> m a
mainClass = elClass "main"

-- | @<span>@ element with given CSS class.
spanClass :: DomBuilder t m => Text -> m a -> m a
spanClass = elClass "span"

-- | @<label>@ element with given CSS class.
labelClass :: DomBuilder t m => Text -> m a -> m a
labelClass = elClass "label"

-- | @<section>@ element with given CSS class.
sectionClass :: DomBuilder t m => Text -> m a -> m a
sectionClass = elClass "section"

-- | @<b>@ element with given CSS class.
bClass :: DomBuilder t m => Text -> m a -> m a
bClass = elClass "b"

-- | @<tr>@ element with given CSS class.
trClass :: DomBuilder t m => Text -> m a -> m a
trClass = elClass "tr"

-- | @<th>@ element with given CSS class.
thClass :: DomBuilder t m => Text -> m a -> m a
thClass = elClass "th"

-- | @<td>@ element with given CSS class.
tdClass :: DomBuilder t m => Text -> m a -> m a
tdClass = elClass "td"

-- | Line break (@<br>@).
br :: DomBuilder t m => m ()
br = el "br" blank

-- | Button element, returns click event.
button
  :: DomBuilder t m
  => Text
  -- ^ Label.
  -> Map Text Text
  -- ^ Attributes.
  -> m (Event t ())
button label attrs = do
  (e, _) <- elAttr' "button" (attrs <> "type" =: "button") $ text label
  return $ domEvent Click e

-- | Button element with only "class" attribute.
buttonClass
  :: DomBuilder t m
  => Text
  -- ^ Label.
  -> Text
  -- ^ CSS class.
  -> m (Event t ())
buttonClass label cls = do
  (e, _) <- elAttr' "button" ("class" =: cls <> "type" =: "button") $ text label
  return $ domEvent Click e

-- | Button element with dynamic attributes.
buttonDynAttrs
  :: (DomBuilder t m, PostBuild t m)
  => Text
  -- ^ Label.
  -> Dynamic t (Map Text Text)
  -- ^ Attributes.
  -> m (Event t ())
buttonDynAttrs label dynAttrs = buttonDyn (constDyn label) dynAttrs

-- | Button element with dynamic label.
buttonDynLabel
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t Text
  -- ^ Label.
  -> Map Text Text
  -- ^ Attributes.
  -> m (Event t ())
buttonDynLabel dynLabel attrs = buttonDyn dynLabel (constDyn attrs)

-- | Button element with dynamic label and attributes.
buttonDyn
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t Text
  -- ^ Label.
  -> Dynamic t (Map Text Text)
  -- ^ Attributes.
  -> m (Event t ())
buttonDyn dynLabel dynAttrs = do
  (e, _) <- elDynAttr' "button" ((<> "type" =: "button") <$> dynAttrs)
    $ dynText dynLabel
  return $ domEvent Click e

-- | Dynamic list widget that creates a list that supports the dynamic
-- addition and removal of items.  This widget is completely general with zero
-- markup-specific choices.  It handles all the event plumbing and lets you
-- completely determine the markup.
dynamicList
  :: MonadWidget t m
  => (Int -> a -> Event t a -> m b)
  -- ^ Widget used to display each item
  -> (b -> Event t ())
  -- ^ Function that gets a remove event from the return value of each item
  -> Event t a
  -- ^ Event that adds a new item to the list that is not based on an
  -- existing item.
  -> [a]
  -- ^ Initial list of items
  -> m (Dynamic t [b])
dynamicList w removeEvent addEvent initList = do
  let initMap = M.fromList $ zip [0..] initList
  rec
    let
      vals = mergeWith (<>)
        [ attachWith addNew (current res) addEvent
        , remove (current res) ]
    res <- listWithKeyShallowDiff initMap vals w
  return $ M.elems <$> res
  where
    addNew m a = M.singleton k (Just a)
      where
        k = if M.null m then 0 else fst (M.findMax m) + 1
    remove res = switch (mergeWith (<>) . fmap f . M.toList <$> res)
      where
        f (k,b) = M.singleton k Nothing <$ removeEvent b

-- | Build widget using text value, if any. Otherwise stays blank.
maybeBlank :: DomBuilder t m => Maybe Text -> (Text -> m ()) -> m ()
maybeBlank mt w = maybe blank w mt

-- | "cursor: pointer" style.
pointer :: Map Text Text
pointer = "style" =: "cursor:pointer;"

table :: DomBuilder t m => m a -> m a
table = el "table"

thead :: DomBuilder t m => m a -> m a
thead = el "thead"

tbody :: DomBuilder t m => m a -> m a
tbody = el "tbody"

th :: DomBuilder t m => m a -> m a
th = el "th"

tr :: DomBuilder t m => m a -> m a
tr = el "tr"

td :: DomBuilder t m => m a -> m a
td = el "td"

form :: DomBuilder t m => Map Text Text -> m a -> m a
form attrs = elAttr "form" attrs

-- | Join event of event into single event.
joinE :: (Reflex t, MonadHold t m) => Event t (Event t a) -> m (Event t a)
joinE = switchHold never

joinEE
  :: (Reflex t, MonadHold t m)
  => Event t (Event t a, Event t b) -> m (Event t a, Event t b)
joinEE = bitraverse joinE joinE . Z.unzip

-- | Separate an event to two sequences based on given predicate.
-- The first element of resulting pair keeps events for which the predicate
-- holds, the second -- those for which it doesn't.
separateE
  :: Reflex t
  => (a -> Bool)
  -> Event t a
  -> (Event t a, Event t a)
separateE p e = (ffilter p e, ffilter (not . p) e)

-- | Separate an event with optional payload.
-- The first event in the resulting pair keeps @Just@s, the seconds keeps
-- @Nothing@s.
separateMaybeE
  :: Reflex t
  => Event t (Maybe a)
  -> (Event t (Maybe a), Event t (Maybe a))
separateMaybeE = separateE isJust

-- | Filter only events with existing payload.
fromMaybeE
  :: Reflex t
  => Event t (Maybe a)
  -> Event t a
fromMaybeE = fmapMaybe id

-- | Separate an event with alternative payload.
-- The first event in the resulting pair keeps @Left@s, the seconds keeps
-- @Right@s.
separateEitherE
  :: Reflex t
  => Event t (Either a b)
  -> (Event t (Either a b), Event t (Either a b))
separateEitherE = separateE isLeft

{-
-- | Assigns HTML content to an element, allows embedding of arbitrary
-- HTML tags, so make sure you trust the source of the second argument
-- @
--   el <- _element_raw . fst <$> el' "span" blank
--   unsafeInnerHTML el "this text will be <i>italicised</i>"
-- @
unsafeInnerHTML :: (MonadJSM m, ToJSVal rawelem) => rawelem -> Text -> m ()
unsafeInnerHTML rel html = liftJSM $ do
  htmlVal <- toJSVal html
  relVal <- toJSVal rel
  JS.setProp "innerHTML" htmlVal (JS.Object relVal)
-}

-- | input element which is changes while edited.
-- It is useful to input dates, phones etc
inputCorrect
  :: (MonadFix m, DomBuilder t m, MonadHold t m)
  => (Text -> Text -> Text) -- ^ old value and new value
  -> InputElementConfig er t (DomBuilderSpace m)
  -> m (InputElement er (DomBuilderSpace m) t)
inputCorrect correct ec = mdo
  ie <- inputElement $ ec & inputElementConfig_setValue .~ eNewValue
  dV <- holdUniqDyn $ value ie
  let
    eNewValue = attachWith correct (current dV)
      $ leftmost [ec ^. inputElementConfig_setValue, updated dV]
  pure ie

-- | input element in form "__/__" (e.g. for credit card)
inputMonth
  :: (MonadFix m, DomBuilder t m, MonadHold t m)
  => InputElementConfig er t (DomBuilderSpace m)
  -> m (InputElement er (DomBuilderSpace m) t)
inputMonth ec = inputCorrect correct2 $ ec
  & initialAttributes %~ (<> "type" =: "text" <> "maxlength" =: "5")
  where
    correct2 (T.length -> ol) (T.foldl' f "" -> nv)
      | ol < 2 && nl == 2 = nv `T.snoc` '/'
      | ol > 3 && nl == 3 = T.init nv
      | otherwise = nv
      where
        nl = T.length nv
    f r c
      | (lr == 2 && isDigit c) = r `T.snoc` '/' `T.snoc` c
      | (lr == 2 && c == '/') || (isDigit c && lr < 5) = T.snoc r c
      | otherwise = r
      where
        lr = T.length r
