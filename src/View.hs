{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}

module View
  ( View(..)
  , render
  )
where

import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.Builder        as T
import           Data.Thyme
import           Lens.Micro.Platform                      ( (&)
                                                          , (%~)
                                                          , both
                                                          )
import           Lucid
import           System.Locale                            ( defaultTimeLocale )
import qualified Text.CSS.Parse                as CSS
import qualified Text.CSS.Render               as CSS
import           Text.Heredoc                             ( str )

import           Model
import           Model.Types                              ( NoMenu(..) )

render :: View -> T.Text
render v = renderText (renderView v)

renderView :: View -> Html ()
renderView View {..} = doctypehtml_ $ do
  sitehead
  body_ $ div_ [class_ "container-fluid main"] $ do
    h1_ $ do
      toHtml day
      " / "
      toHtml (formatTime defaultTimeLocale "%F" date)
    if null restaurants
      then div_ . box_ . h3_ $ ("No lunches " >> toHtml day)
      else div_ $ uncurry
        mappend
        (  splitAt 4 (map renderRest restaurants)
        &  both
        %~ (div_ [class_ "row"] . mconcat)
        )

    sitefooter

renderRest :: Restaurant -> Html ()
renderRest Restaurant {..} = box_ $ do
  h2_ (toHtml name >> " " >> a_ [href_ (T.toStrict url)] "☛")
  ul_ [class_ "food-menu"] $ case menu of
    Left NoLunch -> li_ "No lunch this day!"
    Left _       -> li_ "Something went wrong, " <> a_
      [href_ $ T.toStrict "https://github.com/dtekcth/mat-chalmers/issues/new"]
      "please file an issue."
    Right menus -> mconcat (map renderMenu menus)

renderMenu :: Menu -> Html ()
renderMenu (Menu lunch spec) = li_
  (do
    h3_ (toHtml lunch)
    toHtml spec
  )

box_ :: Html () -> Html ()
box_ = div_ [class_ "col-xs-12 col-sm-6 col-md-3 food"]

sitehead :: Html ()
sitehead = head_
  (do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    link_ [rel_ "icon", type_ "image/png", href_ "icon.png"]
    link_
      [ rel_ "stylesheet"
      , href_ "//fonts.googleapis.com/css?family=Anonymous+Pro:400,700"
      ]
    style_ [] css
    title_ "Lunch at Chalmers"
  )

sitefooter :: Html ()
sitefooter = footer_
  [class_ "col-xs-12 col-sm-12 col-md-12"]
  (do
    "Eat at your own risk :) // "
    "Problems? "
    a_ [href_ "https://github.com/dtekcth/mat-chalmers/issues/new"]
       "File an issue!"
    " // "
    a_ [href_ "https://kortladdning3.chalmerskonferens.se/"] "Top-up your card"
  )

css :: T.Text
css =
  (either error (T.toLazyText . CSS.renderNestedBlocks) . CSS.parseNestedBlocks)
    inlineCSS
 where
  inlineCSS = [str| * {
              |   box-sizing: border-box;
              | }
              |
              | html {
              |   font-size: 10px;
              | }
              |
              | body {
              |   font-family: "Anonymous Pro";
              |   font-size: 14px;
              |   line-height: 1.42857143;
              |   background: #fdf7e2;
              |   color: #333;
              |   margin: 0;
              | }
              |
              | h1,
              | h2,
              | h3 {
              |   line-height: 1.1;
              |   font-weight: 500;
              |   margin: 0;
              |   margin-bottom: 10px;
              | }
              |
              | h1 {
              |   padding-left: 15px;
              |   font-size: 36px;
              |   margin-top: 20px;
              | }
              |
              | h2 {
              |   font-size: 140%;
              |   font-weight: bold;
              | }
              |
              | h3 {
              |   margin-right: 10px;
              |   margin-bottom: 0;
              |   display: inline-block;
              |   font-size: 100%;
              |   font-weight: bold;
              |   color: #bd3613;
              | }
              |
              | p {
              |   font-size: 10vh;
              | }
              |
              | a {
              |   color: #428bca;
              |   text-decoration: none;
              | }
              |
              | ul {
              |   padding: 0;
              |   list-style-type: none;
              |   margin-top: 0;
              |   margin-bottom: 10px;
              | }
              | li {
              |   width: 100%;
              |   font-weight: 400;
              |   padding-bottom: 5px;
              | }
              |
              | .food {
              |   position: relative;
              |   margin-top: 20px;
              | }
              |
              | footer {
              |   font-size: 0.9em;
              |   margin-top: 20px;
              | }
              |
              | /* Emulate Bootstrap Grid layout */
              | .container-fluid {
              |   padding-left: 15px;
              |   padding-right: 15px;
              |   margin-left: auto;
              |   margin-right: auto;
              | }
              | .row {
              |   margin-left: -15px;
              |   margin-right: -15px;
              |   display: flex;
              |   flex-wrap: wrap;
              | }
              | .col-xs-12,
              | .col-sm-12,
              | .col-md-12,
              | .col-sm-6,
              | .col-md-3 {
              |   padding-left: 15px;
              |   padding-right: 15px;
              |   position: relative;
              | }
              | .col-xs-12 {
              |   width: 100%;
              | }
              | @media (min-width: 768px) {
              |   .col-sm-12 {
              |     width: 100%;
              |   }
              |   .col-sm-6 {
              |     width: 50%;
              |   }
              | }
              | @media (min-width: 992px) {
              |   .col-md-3 {
              |     width: 25%;
              |   }
              |   .col-md-12 {
              |     width: 100%;
              |   }
              | }
              |]
