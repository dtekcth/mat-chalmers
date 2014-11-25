{-# LANGUAGE OverloadedStrings #-}
module V 
  ( View (..)
  , render
  ) where

import qualified Data.Text.Lazy as T
import Data.Monoid
import Lucid.Base
import Lucid

import M

-- | What to pass to template.
data View = View
  { restaurants :: [Restaurant]
  , date :: T.Text
  } deriving (Eq, Show)


render :: View -> T.Text
render (View restaurants date) = renderText . doctypehtml_ $ do
  head_ $ do
    with meta_ [charset_ "utf-8"]
    with meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]

    with link_ [rel_ "icon", type_ "image/png", href_ "/icon.png"]
    with link_ [rel_ "stylesheet", href_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.0/css/bootstrap.min.css"]
    with link_ [rel_ "stylesheet", href_ "http://fonts.googleapis.com/css?family=Anonymous+Pro:400,700"]
    with link_ [rel_ "stylesheet", href_ "/style.css"]

    title_ "Mat på Campus Johanneberg"

  body_ $
    with div_ [class_ "container-fluid main"] $ do
      h1_ (toHtmlLazy date)
      div_ (mconcat (map renderRest restaurants))
      with div_ [class_ "col-xs-12 col-sm-12 col-md-12"] $
        with a_ [href_ "https://github.com/adamse/mat-chalmers"] "Kod på Github"


renderRest :: Restaurant -> Html ()
renderRest (Restaurant name menus) = with div_ [class_ "col-xs-12 col-sm-6 col-md-4 food"] $ do
  h2_ (toHtmlLazy name)
  with ul_ [class_ "food-menu"] $
    if null menus
      then li_ "Ingen lunch idag!"
      else mconcat (map renderMenu menus)

renderMenu :: Menu -> Html ()
renderMenu (Menu item spec) = li_ $ do
  h3_ (toHtmlLazy item)
  toHtmlLazy spec

toHtmlLazy :: T.Text -> Html ()
toHtmlLazy = toHtml . T.toStrict
