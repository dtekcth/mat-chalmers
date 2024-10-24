{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module View
  ( View(..)
  , render
  )
where

import qualified Data.Text.Lazy                as T
import           Data.Thyme                               ( defaultTimeLocale
                                                          , formatTime )
import           Lucid

import           Model
import           Model.Types                              ( NoMenu(..) )

render :: View -> T.Text
render v = renderText (renderView v)

renderView :: View -> Html ()
renderView View {..} = doctypehtml_ $ do
  sitehead
  body_ [class_ "bg-[#e9e7e7]"] $ div_ [class_ "px-4 py-4 mx-auto"] $ do
    h1_ [class_ "text-4xl"] $ do
      toHtml day
      " / "
      toHtml (formatTime defaultTimeLocale "%F" date)
    if null restaurants
      then div_ . box_ . h3_ $ ("No lunches " >> toHtml day)
      else div_ [class_ "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-5 justify-evenly"] . mconcat $ map renderRest restaurants
    sitefooter

renderRest :: Restaurant -> Html ()
renderRest Restaurant {..} = box_ $ do
  h2_ [class_ "text-2xl text-orange-500"] (toHtml name >> " " >> a_ [href_ (T.toStrict url)] "☛")
  ul_ [class_ "food-menu"] $ case menu of
    Left NoLunch -> li_ "No lunch this day!"
    Left _       -> li_ "Something went wrong, " <> a_
      [href_ "https://github.com/dtekcth/mat-chalmers/issues/new", class_ "text-orange-500 visited:text-orange-800"]
      "please file an issue."
    Right menus -> mconcat (map renderMenu menus)

renderMenu :: Menu -> Html ()
renderMenu (Menu lunch spec) = li_ [class_ "text-lg"]
  (do
    h3_ [class_ "inline-block font-bold text-orange-500"](toHtml lunch)
    span_ [class_ "invisible"] " "
    toHtml spec
  )

box_ :: Html () -> Html ()
box_ = div_ [class_ "relative mt-5"]

sitehead :: Html ()
sitehead = head_
  (do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    link_ [rel_ "icon", type_ "image/png", href_ "icon.png"]
    link_ [rel_ "stylesheet", href_ "style.css"]
    title_ "Lunch at Chalmers"
  )

sitefooter :: Html ()
sitefooter = footer_
  [class_ "py-6 relative justify-self-end"]
  (do
    "Eat at your own risk :) // "
    "Problems? "
    a_ [href_ "https://github.com/dtekcth/mat-chalmers/issues/new", class_ "text-orange-500 visited:text-orange-800"]
       "File an issue!"
    " // "
    a_ [href_ "https://kortladdning3.chalmerskonferens.se/", class_ "text-orange-500 visited:text-orange-800"] "Top-up your card"
  )
