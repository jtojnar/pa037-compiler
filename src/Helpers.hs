module Helpers where

import Data.Text (Text)
import qualified Data.Text as T

tshow :: Show s => s -> Text
tshow = T.pack . show
