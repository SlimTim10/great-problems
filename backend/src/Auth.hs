{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE OverloadedStrings #-}
module Auth where

import qualified Servant as Servant
import Servant ((:>))

-- type Api = "/" :> ReqBody '[JSON] User :> Post '[JSON] User

-- server :: Servant.Server Api

type MyApi = "nothing" :> Servant.EmptyAPI

-- server :: Servant.Server MyApi _
server = return ()
