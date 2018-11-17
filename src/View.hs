{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}

module View
  ( View (..)
  , render
  ) where

import Data.FileEmbed
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T
import Data.Monoid
import Data.Thyme
import Lens.Micro.Platform ((&), (%~), both)
import Lucid
import System.Locale (defaultTimeLocale)
import qualified Text.CSS.Parse as CSS
import qualified Text.CSS.Render as CSS

import Model
import Model.Types (NoMenu(..))

render :: View -> T.Text
render v = renderText (renderView v)

renderView :: View -> Html ()
renderView View{..} =
  doctypehtml_ $ do
    sitehead
    body_ $ do
      div_ [class_ "container-fluid main"] $ do
        h1_ $ do
          toHtml day
          " / "
          toHtml (formatTime defaultTimeLocale "%F" date)
        if null restaurants
          then div_ . box_ . h3_ $ ("No lunches " >> toHtml day)
          else div_ (
            (uncurry mappend)
              ((splitAt 4 (map renderRest restaurants)) & both %~ (div_ [class_ "row"] . mconcat))
          )
        sitefooter

renderRest :: Restaurant -> Html ()
renderRest Restaurant {..} =
  box_ $ do
    h2_ (toHtml name >> " " >> a_ [href_ (T.toStrict url)] "☛")
    ul_ [class_ "food-menu"] $
      case menu of
        Left NoLunch -> li_ "No lunch this day!"
        Left (SomethingWrong _) -> li_ ("Something went wrong, " <>
          (a_ [href_ $ T.toStrict "https://github.com/adamse/mat-chalmers/issues/new"] "please file an issue."))
        Right menus -> mconcat (map renderMenu menus)

renderMenu :: Menu -> Html ()
renderMenu (Menu lunch spec) =
  li_ (do h3_ (toHtml lunch)
          toHtml spec)

box_ :: Html () -> Html ()
box_ = div_ [class_ "col-xs-12 col-sm-6 col-md-3 food"]

sitehead :: Html ()
sitehead =
  head_ (do meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport"
                  ,content_ "width=device-width, initial-scale=1"]
            link_ [rel_ "icon",type_ "image/png",href_ "icon.png"]
            link_ [rel_ "stylesheet"
                  ,href_ "//maxcdn.bootstrapcdn.com/bootstrap/3.3.0/css/bootstrap.min.css"]
            link_ [rel_ "stylesheet"
                  ,href_ "//fonts.googleapis.com/css?family=Anonymous+Pro:400,700"]
            style_ [] css
            title_ "Lunch at Chalmers")

sitefooter :: Html ()
sitefooter =
  footer_ [class_ "col-xs-12 col-sm-12 col-md-12"]
          (do "Eat at your own risk :) // "
              "Problems? "
              a_ [href_ "https://github.com/adamse/mat-chalmers/issues/new"] "File an issue!"
              " // "
              a_ [href_ "https://kortladdning3.chalmerskonferens.se/"] "Top-up your card")

css :: T.Text
css =
  (either error (T.toLazyText . CSS.renderNestedBlocks) . CSS.parseNestedBlocks)
    $(embedStringFile "static/style.css")
