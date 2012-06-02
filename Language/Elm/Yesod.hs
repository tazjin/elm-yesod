{-# LANGUAGE QuasiQuotes, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{- | This module provides a function for compiling Elm source code into a Yesod widget.

     You  need to define an instance of 'YesodElm', which will specify
     where to find the elm-min.js file.

     For example:
    @
     instance YesodElm App where
       urlElmJs _ = Right $ "http://link.to/elm-min.js"
    @

     or

    @
     instance YesodElm App where
       urlElmJs _ = Left $ StaticR js_elm_min_js
    @

     A full example implementation is provided in the examples folder of the Elm github repository
     at <https://github.com/tazjin/Elm/tree/master/Examples/elm-yesod>.
-}
module Language.Elm.Yesod (
        elmWidget
      , YesodElm (..)
      , ElmUrl) where

import Control.Monad (liftM)
import Text.Blaze (preEscapedToMarkup)
import Text.Julius
import Yesod.Core (Route (..), Yesod(..), ScriptLoadPosition(..))
import Yesod.Handler (getUrlRenderParams, GHandler (..))
import Yesod.Widget
import Yesod.Handler (lift, getYesod)
import Language.Elm
import Language.Elm.Quasi

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

class YesodElm master where
    -- | The location of the elm-min.js file. This can be either a type-safe
    -- route (@Left@) or a raw string (@Right@).
    urlElmJs :: a -> Either (Route master) TS.Text

instance (Yesod master, YesodElm master, render ~ RenderFn (Route master))
  => ToWidget sub master (render -> Elm) where
    toWidget = elmWidget

-- |elmWidget returns a Yesod widget from some Elm source code
--  with URL interpolation.
elmWidget :: (Yesod master, YesodElm master)
          => ElmUrl (Route master) -- ^ Elm source code
          -> GWidget sub master()
elmWidget source = do
  urlF <- lift getUrlRenderParams
  master <- lift getYesod
  mkElmWidget source urlF (jsLoader master)
  addScriptEither $ urlElmJs master


mkElmWidget :: Yesod master
            => ElmUrl (Route master)   -- ^ Elm source code
            -> RenderFn (Route master) -- ^ URL rendering function
            -> ScriptLoadPosition master
            -> GWidget sub master ()
mkElmWidget source urlFn jsl =
  let (html, css, js) = toParts (urlFn, source)
      initscript = [julius| Dispatcher.initialize(); |]
  in do toWidgetHead css
        toWidget [julius| #{js} |]
        toWidget html
        case jsl of
          BottomOfBody -> toWidget initscript  -- insert as last script
          otherwise -> toWidgetBody initscript -- insert in body instead of head


-- | Return type of template-reading functions.
type ElmUrl url = (url -> [(TS.Text, TS.Text)] -> TS.Text) -> Elm

-- | Readability synonym for render function
type RenderFn url = (url -> [(TS.Text, TS.Text)] -> TS.Text)
