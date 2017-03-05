module Container where

import Prelude
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.FileSelector.Component as FileSelector
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import DOM (DOM)
import Data.Array (head)
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType(..))
import Halogen.Component.ChildPath (type (\/), type (<\/>))
import Halogen.Query (liftAff)

type State = { txt :: Maybe String
             , txts :: Array String
             , img :: Maybe String
             , imgs :: Array String
             }

type ChildQuery = FileSelector.Query <\/> FileSelector.Query <\/> FileSelector.Query <\/> FileSelector.Query <\/> Const Void
type ChildSlot = FileSelector.Slot \/ FileSelector.Slot \/ FileSelector.Slot \/ FileSelector.Slot \/ Void
type CompEff e = Aff (dom :: DOM | e)
data Query a = HandleTextSelector FileSelector.Message a
             | HandleTextsSelector FileSelector.Message a
             | HandleImageSelector FileSelector.Message a
             | HandleImagesSelector FileSelector.Message a

component :: forall m. H.Component HH.HTML Query Unit Void (CompEff m)
component =
  H.parentComponent
    { initialState: const { txt : Nothing
                          , txts : []
                          , img: Nothing
                          , imgs: []
                          }
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (CompEff m)
  render state =
    HH.div_ $ []
    <> sel CP.cp1 HandleTextSelector [ HP.accept (MediaType "text/plain")
                                     ]
    <> maybeToArray (txt <$> state.txt)
    <> sel CP.cp2 HandleTextsSelector [ HP.accept (MediaType "text/plain")
                                      , HP.multiple true
                                      ]
    <> (txt <$> state.txts)
    <> sel CP.cp3 HandleImageSelector [ HP.accept (MediaType "image/*")
                                      , HP.capture true
                                      ]
    <> maybeToArray (img <$> state.img)
    <> sel CP.cp4 HandleImagesSelector [ HP.accept (MediaType "image/*")
                                       , HP.multiple true
                                       , HP.capture true
                                       ]
    <> (img <$> state.imgs)    
    where sel cp handler props = [ HH.div_ [ HH.slot' cp FileSelector.Slot (FileSelector.selector props) unit (HE.input handler) ] ]
          txt c = HH.div_ [ HH.text c ]
          img c = HH.div_ [ HH.img [ HP.src c ] ]
          maybeToArray :: forall a. Maybe a -> Array a
          maybeToArray = maybe [] pure

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (CompEff m)
  eval (HandleTextSelector (FileSelector.Selected s) next) = do
    contents <- liftAff $ FileSelector.texts s
    H.modify $ (_ { txt = head contents })
    pure next
  eval (HandleTextsSelector (FileSelector.Selected s) next) = do
    contents <- liftAff $ FileSelector.texts s
    H.modify $ (_ { txts = contents })
    pure next
  eval (HandleImageSelector (FileSelector.Selected s) next) = do
    contents <- liftAff $ FileSelector.urls s
    H.modify $ (_ { img = head contents })
    pure next
  eval (HandleImagesSelector (FileSelector.Selected s) next) = do
    contents <- liftAff $ FileSelector.urls s
    H.modify $ (_ { imgs = contents })
    pure next
