module Halogen.FileSelector.Component where
import Prelude
import Control.Monad.State as CMS
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Control.MonadZero (guard)
import DOM (DOM)
import DOM.Event.Event (target)
import DOM.Event.Types (Event)
import DOM.File.FileList (item, length)
import DOM.File.Types (Blob, FileList, fileToBlob)
import DOM.HTML.HTMLInputElement (files)
import DOM.HTML.Indexed (HTMLinput)
import Data.Array ((..))
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing)
import Data.Nullable (toMaybe)
import Halogen.HTML.Properties (IProp)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

type State = Array Blob

data Query a  = Select Event a
              | GetSelection (State -> a)

data Message = Selected (Array Blob)

type Effects e = (Aff (dom :: DOM | e))

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

selector :: forall e. Array (IProp HTMLinput (Query Unit)) -> H.Component HH.HTML Query Unit Message (Effects e)
selector props =
  H.component
    { initialState: const []
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render st = HH.input $ [ HP.type_ HP.InputFile
                         , HE.onChange (HE.input Select)
                         ]  <> props

  eval :: Query ~> H.ComponentDSL State Query Message (Effects e)
  eval (Select event next) = do
    lst <- H.liftEff $ files $ unsafeCoerce $ target event
    let blobs :: FileList -> Array Blob
        blobs fs = do
          i <- 0 .. (length fs - 1)
          let f = toMaybe $ item i fs
          guard $ not isNothing f
          pure $ fileToBlob $ unsafePartial fromJust f
        sel = fromMaybe [] $ blobs <$> toMaybe lst
    CMS.put sel
    H.raise $ Selected sel
    pure next      
  eval (GetSelection reply) = do
    s <- CMS.get
    pure (reply s)

