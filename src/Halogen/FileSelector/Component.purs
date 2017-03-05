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
import DOM.File.FileList (item, length)
import DOM.File.FileReader.Aff (readAsArrayBuffer, readAsDataURL, readAsText)
import DOM.File.Types (Blob, FileList, fileToBlob)
import DOM.HTML.Indexed (HTMLinput)
import Data.Array ((..))
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Nullable (toMaybe)
import Data.Traversable (traverse)
import Halogen.HTML.Properties (IProp)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

type State = FileList

data Query a  = Select FileList a
              | GetSelection (State -> a)

data Message = Selected FileList

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

selector :: forall r m. Array (IProp HTMLinput r) -> H.Component HH.HTML Query Unit Message m
selector props =
  H.component
    { initialState: const $ unsafeCoerce []
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render st = HH.input $ [ HP.type_ HP.InputFile
                         , HE.onFilesSelected (HE.input Select)
                         ] <> unsafeCoerce props

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval (Select s next) = do
    CMS.put s
    H.raise $ Selected s
    pure next      
  eval (GetSelection reply) = do
    s <- CMS.get
    pure (reply s)

blobs :: FileList -> Array Blob
blobs fs = do
  i <- 0 .. (length fs - 1)
  let f = toMaybe $ item i fs
  guard $ not isNothing f
  pure $ fileToBlob $ unsafePartial fromJust f

texts :: forall e. FileList -> Aff (dom :: DOM | e) (Array String)
texts = traverse readAsText <<< blobs
          
urls :: forall e. FileList -> Aff (dom :: DOM | e) (Array String)
urls = traverse readAsDataURL <<< blobs

buffers :: forall e. FileList -> Aff (dom :: DOM | e) (Array ArrayBuffer)
buffers = traverse readAsArrayBuffer <<< blobs

