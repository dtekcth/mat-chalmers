{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module V
  ( View (..)
  , render
  ) where

-- import           Data.Monoid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString.Lazy as B
import           Data.Thyme
import           Lucid
import           System.Locale (defaultTimeLocale)
import qualified Text.CSS.Parse as CSS
import qualified Text.CSS.Render as CSS
import           Data.ByteString.Base64.URL as B64
import           Data.FileEmbed
import           Data.Monoid

import           M

render :: View -> T.Text
render v = renderText (renderView v)

renderView :: View -> Html ()
renderView (View {..}) =
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
          else div_ (mconcat (map renderRest restaurants))
        sitefooter
      toHtmlRaw analytics

renderRest :: Restaurant -> Html ()
renderRest (Restaurant {..}) =
  box_
    (do h2_ (toHtml name >> " " >> a_ [href_ (T.toStrict url)] "☛")
        ul_
          [class_ "food-menu"]
          (if null menu
             then li_ "No lunch this day!"
             else mconcat (map renderMenu menu)))

renderMenu :: Menu -> Html ()
renderMenu (Menu{..}) =
  li_ (do h3_ (toHtml lunch)
          toHtml spec)

box_ :: Html () -> Html ()
box_ = div_ [class_ "col-xs-12 cols-sm-6 col-md-4 food"]

sitehead :: Html ()
sitehead =
  head_ (do meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport"
                  ,content_ "width=device-width, initial-scale=1"]
            link_ [rel_ "icon",type_ "image/png",href_ icon]
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
              a_ [href_ "https://github.com/adamse/mat-chalmers"] "Source at Github"
              " // "
              a_ [href_ "http://kortladdning3.chalmerskonferens.se/"] "Charge your card")

analytics :: T.Text
analytics = "<script>(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)})(window,document,'script','//www.google-analytics.com/analytics.js','ga');ga('create', 'UA-60251317-1', 'auto');ga('send', 'pageview');</script>"

css :: T.Text
css =
  (either error (T.toLazyText . CSS.renderNestedBlocks) . CSS.parseNestedBlocks)
    $(embedStringFile "static/style.css")

icon =
  "data:image/png;base64," <>
  (T.toStrict . T.decodeUtf8 . B.fromStrict . B64.encode $ $(embedFile "static/icon.png"))
