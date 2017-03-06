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
import DOM.File.FileReader.Aff (readAsDataURL, readAsText)
import Data.Array (head)
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType(..))
import Data.Traversable (traverse)
import Halogen.Component.ChildPath (type (\/), type (<\/>))
import Halogen.Query (liftAff)

type State = { txt :: Maybe String
             , txts :: Array String
             , img :: Maybe String
             , imgs :: Array String
             }
type ChildQuery = FS.Query <\/> FS.Query <\/> FS.Query <\/> FS.Query <\/> Const Void
type ChildSlot = FS.Slot \/ FS.Slot \/ FS.Slot \/ FS.Slot \/ Void
type Effects e = Aff (dom :: DOM | e)
data Query a = HandleTextSelector FS.Message a
             | HandleTextsSelector FS.Message a
             | HandleImageSelector FS.Message a
             | HandleImagesSelector FS.Message a

component :: forall e. H.Component HH.HTML Query Unit Void (Effects e)
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

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Effects e)
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
                                      ]
    <> maybeToArray (img <$> state.img)
    <> sel CP.cp4 HandleImagesSelector [ HP.accept (MediaType "image/*")
                                       , HP.multiple true
                                       ]
    <> (img <$> state.imgs)    
    where sel cp handler props = [ HH.div_ [ HH.slot' cp FS.Slot (FS.selector props) unit (HE.input handler) ] ]
          txt c = HH.div_ [ HH.text c ]
          img c = HH.div_ [ HH.img [ HP.src c ] ]
          maybeToArray :: forall a. Maybe a -> Array a
          maybeToArray = maybe [] pure

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Effects e)
  eval (HandleTextSelector (FS.Selected blobs) next) = do
    contents <- liftAff $ traverse readAsText blobs
    H.modify $ (_ { txt = head contents })
    pure next
  eval (HandleTextsSelector (FS.Selected blobs) next) = do
    contents <- liftAff $ traverse readAsText blobs
    H.modify $ (_ { txts = contents })
    pure next
  eval (HandleImageSelector (FS.Selected blobs) next) = do
    contents <- liftAff $ traverse readAsDataURL blobs
    H.modify $ (_ { img = head contents })
    pure next
  eval (HandleImagesSelector (FS.Selected blobs) next) = do
    contents <- liftAff $ traverse readAsDataURL blobs
    H.modify $ (_ { imgs = contents })
    pure next
