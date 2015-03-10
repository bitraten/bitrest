{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Model.User where

import           Data.Text    (Text)
import           Servant

newtype AccessToken = AccessToken Text

data Role = Guest | Member | Admin

data WithRole

instance HasServer a => HasServer (WithRole :> a) where
    type Server (WithRole :> a) = Role -> Server a
    route Proxy subserver request respond =
        route (Proxy :: Proxy a) (subserver Guest) request respond
