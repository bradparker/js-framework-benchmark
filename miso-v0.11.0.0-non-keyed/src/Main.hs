{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Monad (mapM)
import           Data.Bool     (bool)
import           Data.Foldable (foldr)
import           Data.List     (intersperse, length, zipWith)
import           Data.Monoid   (mconcat, mempty, (<>))
import           Data.Vector   (Vector, (!))
import qualified Data.Vector   as Vector
import           Miso          (App (..), Attribute, Effect, View, a_, button_,
                                class_, defaultEvents, div_, h1_, href_, id_,
                                noEff, onClick, span_, startApp, table_, tbody_,
                                td_, text, tr_, type_, (<#))
import           Miso.String   (MisoString, ms)

main :: IO ()
main =
  startApp
    App
    { view = updateView
    , update = updateModel
    , model = initialModel
    , initialAction = NoOp
    , subs = []
    , mountPoint = Nothing
    , events = defaultEvents
    }

adjectives :: Vector MisoString
adjectives =
  Vector.fromList
    [ "pretty"
    , "large"
    , "big"
    , "small"
    , "tall"
    , "short"
    , "long"
    , "handsome"
    , "plain"
    , "quaint"
    , "clean"
    , "elegant"
    , "easy"
    , "angry"
    , "crazy"
    , "helpful"
    , "mushy"
    , "odd"
    , "unsightly"
    , "adorable"
    , "important"
    , "inexpensive"
    , "cheap"
    , "expensive"
    , "fancy"
    ]

colours :: Vector MisoString
colours =
  Vector.fromList
    [ "red"
    , "yellow"
    , "blue"
    , "green"
    , "pink"
    , "brown"
    , "purple"
    , "brown"
    , "white"
    , "black"
    , "orange"
    ]

nouns :: Vector MisoString
nouns =
  Vector.fromList
    [ "table"
    , "chair"
    , "house"
    , "bbq"
    , "desk"
    , "car"
    , "pony"
    , "cookie"
    , "sandwich"
    , "burger"
    , "pizza"
    , "mouse"
    , "keyboard"
    ]

buttons :: [(MisoString, MisoString, Action)]
buttons =
  [ ("run", "Create 1,000 rows", Create 1000 Replace)
  , ("runlots", "Create 10,000 rows", Create 10000 Replace)
  , ("add", "Append 1,000 rows", Create 1000 Append)
  , ("update", "Update every 10th row", UpdateEvery 10)
  , ("clear", "Clear", Clear)
  , ("swaprows", "Swap Rows", Swap 1 998)
  ]

button :: (MisoString, MisoString, Action) -> View Action
button (buttonId, label, action) =
  div_
    [class_ "col-sm-6 smallpad"]
    [ button_
        [ type_ "button"
        , class_ "btn btn-primary btn-block"
        , id_ buttonId
        , onClick action
        ]
        [text label]
    ]

row :: Maybe Int -> Row -> View Action
row selected Row {_id, label} =
  tr_
    (bool [] [class_ "danger"] (selected == Just _id))
    [ td_ [class_ "col-md-1"] [text (ms (show _id))]
    , td_
        [class_ "col-md-4"]
        [a_ [href_ "#", onClick (Select _id)] [text label]]
    , td_
        [class_ "col-md-1"]
        [ a_
            [href_ "#", onClick (Remove _id)]
            [span_ [class_ "glyphicon glyphicon-remove"] []]
        ]
    , td_ [class_ "col-md-6"] []
    ]

updateView :: Model -> View Action
updateView Model {rows, selected} =
  div_
    [class_ "container"]
    [ div_
        [class_ "jumbotron"]
        [ div_
            [class_ "row"]
            [ div_ [class_ "col-md-6"] [h1_ [] [text "Miso 0.11.0.0"]]
            , div_ [class_ "col-md-6"] (map button buttons)
            ]
        ]
    , table_
        [class_ "table table-hover table-striped test-data"]
        [tbody_ [] (map (row selected) rows)]
    , span_ [class_ "preloadicon glyphicon glyphicon-remove"] []
    ]

data Action
  = NoOp
  | Create Int
           ([Row] -> Action)
  | Append [Row]
  | Replace [Row]
  | UpdateEvery Int
  | Clear
  | Swap Int
         Int
  | Remove Int
  | Select Int

foreign import javascript unsafe "$r = h$rand();" getRand :: IO Int

updateModel :: Action -> Model -> Effect Action Model
updateModel (Create amount action) model@Model {lastId} =
  model <#
  (action <$>
   mapM
     (((generateRow <$> getRand <*> getRand <*> getRand) <*>) . pure)
     [lastId + 1 .. lastId + amount])
updateModel (Replace newRows) model@Model {rows, lastId} =
  noEff model {rows = newRows, lastId = lastId + length newRows}
updateModel (Append newRows) model@Model {rows, lastId} =
  noEff model {rows = rows <> newRows, lastId = lastId + length newRows}
updateModel (Select _id) model = noEff model {selected = Just _id}
updateModel (UpdateEvery amount) model@Model {rows} =
  noEff model {rows = zipWith updateRow [0 ..] rows}
updateModel (Swap one two) model@Model {rows} =
  noEff model {rows = swap one two rows}
updateModel (Remove target) model@Model {rows} =
  noEff model {rows = filter ((target /=) . _id) rows}
updateModel Clear model@Model {rows} = noEff model {rows = mempty}
updateModel NoOp model = noEff model

swap :: Int -> Int -> [a] -> [a]
swap one two xs = zipWith insert [0 ..] xs
  where
    insert i x
      | i == two = xs !! one
      | i == one = xs !! two
      | otherwise = x

generateRow :: Int -> Int -> Int -> Int -> Row
generateRow adjectiveIndex colourIndex nounIndex _id =
  Row {_id = _id, label = adjective <> " " <> colour <> " " <> noun}
  where
    adjective = adjectives ! (adjectiveIndex `mod` Vector.length adjectives)
    colour = colours ! (colourIndex `mod` Vector.length colours)
    noun = nouns ! (nounIndex `mod` Vector.length nouns)

updateRow :: Int -> Row -> Row
updateRow index row@Row {label}
  | index `mod` 10 == 0 = row {label = label <> " !!!"}
  | otherwise = row

data Model = Model
  { rows     :: [Row]
  , lastId   :: !Int
  , selected :: Maybe Int
  } deriving (Show, Eq, Ord)

data Row = Row
  { _id   :: !Int
  , label :: !MisoString
  } deriving (Show, Eq, Ord)

initialModel :: Model
initialModel = Model mempty 0 Nothing
