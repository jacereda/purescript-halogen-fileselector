module Container where

import Prelude
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.FileSelector.Component as FS
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

type ChildQuery = FS.Query <\/> FS.Query <\/> FS.Query <\/> FS.Query <\/> Const Void
type ChildSlot = FS.Slot \/ FS.Slot \/ FS.Slot \/ FS.Slot \/ Void
type CompEff e = Aff (dom :: DOM | e)
data Query a = HandleTextSelector FS.Message a
             | HandleTextsSelector FS.Message a
             | HandleImageSelector FS.Message a
             | HandleImagesSelector FS.Message a

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
    where sel cp handler props = [ HH.div_ [ HH.slot' cp FS.Slot (FS.selector props) unit (HE.input handler) ] ]
          txt c = HH.div_ [ HH.text c ]
          img c = HH.div_ [ HH.img [ HP.src c ] ]
          maybeToArray :: forall a. Maybe a -> Array a
          maybeToArray = maybe [] pure

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (CompEff m)
  eval (HandleTextSelector (FS.Selected s) next) = do
    contents <- liftAff $ FS.texts s
    H.modify $ (_ { txt = head contents })
    pure next
  eval (HandleTextsSelector (FS.Selected s) next) = do
    contents <- liftAff $ FS.texts s
    H.modify $ (_ { txts = contents })
    pure next
  eval (HandleImageSelector (FS.Selected s) next) = do
    contents <- liftAff $ FS.urls s
    H.modify $ (_ { img = head contents })
    pure next
  eval (HandleImagesSelector (FS.Selected s) next) = do
    contents <- liftAff $ FS.urls s
    H.modify $ (_ { imgs = contents })
    pure next
