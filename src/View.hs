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
    link_ [rel_ "shortcut icon", id_ "icon", type_ "image/png", href_ "data:image/png;base64,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"]
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
