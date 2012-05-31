{-# LANGUAGE QuasiQuotes, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{- | This module provides a function for compiling Elm source code into a Yesod widget.
     In order to use this with your Yesod app, you need to define a defaultLayout like
     function that embeds the elm-min.js file /in the <head> tag/.

     For example, you could modify your Yesod instance as follows:

   > instance Yesod App where
   >    jsLoader _ = BottomOfHeadBlocking -- moves JS to the <head> tag

     You also need to define an instance of 'YesodElm', which will specify
     where to find the elm-min.js file.

     A full example implementation is provided in the examples folder of the Elm github repository.
-}
module Language.Elm.Yesod (
        elmWidget
      , YesodElm (..)
      , ElmUrl) where

import Control.Monad (liftM)
import Text.Blaze (preEscapedToMarkup)
import Text.Julius
import Yesod.Core (Route (..))
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

instance (YesodElm master, render ~ RenderFn (Route master))
  => ToWidget sub master (render -> Elm) where
    toWidget = elmWidget

-- |elmWidget returns a Yesod widget from some Elm source code
--  with URL interpolation.
elmWidget :: YesodElm master
          => ElmUrl (Route master) -- ^ Elm source code
          -> GWidget sub master()
elmWidget source = do
  urlF <- lift getUrlRenderParams
  mkElmWidget source urlF
  master <- lift getYesod
  addScriptEither $ urlElmJs master


mkElmWidget :: ElmUrl (Route master)   -- ^ Elm source code
            -> RenderFn (Route master) -- ^ URL rendering function
            -> GWidget sub master ()
mkElmWidget source urlFn =
  let (html, css, js) = toParts (urlFn, source) in
  do toWidgetHead css
     toWidgetHead [julius| #{js} |]
     toWidgetBody html


-- | Return type of template-reading functions.
type ElmUrl url = (url -> [(TS.Text, TS.Text)] -> TS.Text) -> Elm

-- | Readability synonym for render function
type RenderFn url = (url -> [(TS.Text, TS.Text)] -> TS.Text)
