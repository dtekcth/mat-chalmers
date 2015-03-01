{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module V
  ( View (..)
  , render
  ) where

import           Data.Monoid
import qualified Data.Text.Lazy as T
import           Lucid

import           M

render :: View -> T.Text
render v = renderText (renderView v)

renderView :: View -> Html ()
renderView (View{..}) =
  doctypehtml_
    (do head_ (do meta_ [charset_ "utf-8"]
                  meta_ [name_ "viewport"
                        ,content_ "width=device-width, initial-scale=1"]
                  link_ [rel_ "icon",type_ "image/png",href_ "/icon.png"]
                  link_ [rel_ "stylesheet"
                        ,href_ "//maxcdn.bootstrapcdn.com/bootstrap/3.3.0/css/bootstrap.min.css"]
                  link_ [rel_ "stylesheet"
                        ,href_ "//fonts.googleapis.com/css?family=Anonymous+Pro:400,700"]
                  link_ [rel_ "stylesheet",href_ "/style.css"]
                  title_ "Mat p\229 Campus Johanneberg")
        body_ (do div_ [class_ "container-fluid main"]
                       (do h1_ (toHtml date)
                           div_ (mconcat (map renderRest restaurants))
                           footer_ [class_ "col-xs-12 col-sm-12 col-md-12"]
                                   (do a_ [href_ "https://github.com/adamse/mat-chalmers"]
                                          "Kod p\229 Github"
                                       " // Stavfel och andra konstigheter \228r n\229gon annans fel."))
                  toHtmlRaw analytics))

renderRest :: Restaurant -> Html ()
renderRest (Restaurant{..}) =
  box (do h2_ (toHtml name)
          ul_ [class_ "food-menu"]
              (if null menu
                  then li_ "Ingen lunch!"
                  else mconcat (map renderMenu menu)))

renderMenu :: Menu -> Html ()
renderMenu (Menu{..}) =
  li_ (do h3_ (toHtml lunch)
          toHtml spec)

box :: Html () -> Html ()
box = div_ [class_ "col-xs-12 cols-sm-6 col-md-4 food"]

analytics :: T.Text
analytics = "<script>\
\(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){\
\(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),\
\m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)\
\})(window,document,'script','//www.google-analytics.com/analytics.js','ga');\
\ga('create', 'UA-60251317-1', 'auto');\
\ga('send', 'pageview');\
\</script>"
