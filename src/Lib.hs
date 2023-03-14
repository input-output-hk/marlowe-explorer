{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp
  , app
  ) where

import Control.Monad.IO.Class ( liftIO )
import Control.Newtype.Generics ( op )
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Servant ( serve, Proxy(..), type (:>), Get, type (:<|>) ((:<|>)), QueryParam )
import Servant.HTML.Blaze ( HTML )
import Servant.Server ( hoistServer )

import Explorer.SharedContractCache ( ContractListCache )
import Explorer.Web.ContractListView ( ContractListView (..), contractListView )
import Explorer.Web.ContractView ( ContractView (..), contractView )
import Language.Marlowe.Runtime.Background ( start )
import Opts ( Options (optExplorerPort), ExplorerPort (..), mkUrlPrefix )

startApp :: Options -> IO ()
startApp opts = do
  let eport = op ExplorerPort . optExplorerPort $ opts
  putStrLn $ "Marlowe Explorer server started, available at localhost:"
    <> show eport
  contractListCache <- start $ mkUrlPrefix opts
  run eport $ app opts contractListCache

type API
     = Get '[HTML] ContractListView  -- Initial "index" page, http://HOST:PORT/
  :<|> "listContracts" :> QueryParam "page" Int :> Get '[HTML] ContractListView
  :<|> "contractView" :> QueryParam "tab" String :> QueryParam "contractId" String :> Get '[HTML] ContractView

app :: Options -> ContractListCache -> Application
app opts contractListCache =
  serve (Proxy :: Proxy API) $
    hoistServer (Proxy :: Proxy API) liftIO
    (contractListView opts contractListCache Nothing
    :<|> contractListView opts contractListCache
    :<|> contractView opts)
