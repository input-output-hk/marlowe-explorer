{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Marlowe.Runtime.Types.ContractsJSON
  ( ContractInList(..)
  , ContractList(..)
  , ContractLinks(..)
  , Resource(..)
  , getContracts
  )
  where

import Control.Exception ( try )
import Control.Monad.Except ( ExceptT, runExceptT, throwError )
import Control.Monad.Reader ( ReaderT, asks, runReaderT )
import Control.Monad.Trans ( liftIO )
import Data.Aeson ( withObject, (.:), FromJSON(parseJSON), eitherDecode, Value )
import Data.ByteString ( ByteString )
import Data.List ( foldl' )
import Data.Time.Clock ( UTCTime, getCurrentTime )
import Network.HTTP.Simple ( HttpException, Request, parseRequest,
  getResponseBody, httpLBS, getResponseHeader, setRequestHeader,
  setRequestMethod )

import Language.Marlowe.Runtime.AdjustTip ( BuildChain (Building, BuildDone),
  Seq, (><), empty, fromList, processTip )
import Language.Marlowe.Runtime.Types.Common ( Block )
import Data.Aeson.Types (Parser)

data Resource = Resource
  { block :: Block
  , continuations :: Maybe String
  , contractId :: String
  , roleTokenMintingPolicyId :: String
  , status :: String
  , version :: String
  } deriving (Show, Eq)

instance FromJSON Resource where
  parseJSON :: Value -> Parser Resource
  parseJSON = withObject "Resource" $ \o ->
    Resource <$> o .: "block"
             <*> o .: "continuations"
             <*> o .: "contractId"
             <*> o .: "roleTokenMintingPolicyId"
             <*> o .: "status"
             <*> o .: "version"

data ContractLinks = ContractLinks
  { contract :: String
  , transactions :: String
  } deriving (Show, Eq)

instance FromJSON ContractLinks where
  parseJSON :: Value -> Parser ContractLinks
  parseJSON = withObject "ContractLinks" $ \o ->
    ContractLinks <$> o .: "contract"
                  <*> o .: "transactions"

data ContractInList = ContractInList
  { links :: ContractLinks
  , resource :: Resource
  } deriving (Show, Eq)

instance FromJSON ContractInList where
  parseJSON :: Value -> Parser ContractInList
  parseJSON = withObject "ContractInList" $ \o ->
    ContractInList <$> o .: "links"
                   <*> o .: "resource"

newtype ResultList = ResultList
  { results :: [ContractInList]
  } deriving (Show, Eq)

instance FromJSON ResultList where
  parseJSON :: Value -> Parser ResultList
  parseJSON = withObject "ResultList" $ \o ->
    ResultList <$> o .: "results"

data ContractList = ContractList
  { clRetrievedTime :: Maybe UTCTime
  , clContracts :: Seq ContractInList
  }
  deriving (Show, Eq)

data Range
  = Start
  | Next ByteString
  | Done
  deriving (Eq, Show)

type GetContracts a = ReaderT (String, Seq ContractInList) (ExceptT String IO) a

runGetContracts :: (String, Seq ContractInList) -> GetContracts a -> IO (Either String a)
runGetContracts env ev = runExceptT $ runReaderT ev env


getContracts :: String -> Seq ContractInList -> IO (Either String ContractList)
getContracts endpoint lOldChain = do
  eresult <- runGetContracts (endpoint, lOldChain) $ getContracts' (empty, Start)
  now <- getCurrentTime
  return $ do (contracts, _) <- eresult
              return (ContractList { clRetrievedTime = Just now
                                   , clContracts = contracts })


-- This code has two criteria when trying to get the latest Marlowe contract
-- data to add to what we already have cached.
--
-- 1. We need to keep asking the Marlowe Runtime for pages of contract data
--    until the Next-Range header indicates there are no more.
-- 2. Regardless of the Runtime paging above, we need to only download
--    contracts from the tip backwards until we locate the point where our
--    prior contract list has them already. This will handle both new contracts
--    since the last time and also any rollbacks that have occurred, because we
--    trim off the cached chain prior to the point where it matches new data.
--
-- Point 1 is what the Range (Start | Next ... | Done) data structure is for
-- Point 2 is what the Language.Marlowe.Runtime.AdjustTip.BuildChain
-- (Building | BuildDone) data structure is for

getContracts' :: (Seq ContractInList, Range) -> GetContracts (Seq ContractInList, Range)

getContracts' t@(_acc, Done) = return t

getContracts' (acc, range) = do
  (nextContracts, nextRange) <- contractsRESTCall range
  oldChain <- asks snd
  case processTip oldChain nextContracts of
    -- Note setting Range to Done here because we have enough info and can stop
    BuildDone endOfChain -> getContracts' (acc >< endOfChain, Done)
    Building newTip -> getContracts' (acc >< newTip, nextRange)


setRangeHeader :: Range -> Request -> Request
setRangeHeader (Next bs) = setRequestHeader "Range" [bs]
setRangeHeader _ = id

parseRangeHeader :: [ByteString] -> Range
parseRangeHeader [bs] = Next bs
parseRangeHeader _ = Done

contractsRESTCall :: Range -> GetContracts (Seq ContractInList, Range)
contractsRESTCall range = do
  endpoint <- asks fst
  initialRequest <- parseRequest $ endpoint <> "contracts"
  let request = foldl' (flip id) initialRequest
        [ setRequestMethod "GET"
        , setRequestHeader "Accept" ["application/json"]
        , setRangeHeader range
        ]
  eresponse <- liftIO . try $ httpLBS request
  case eresponse of
    Left exception -> throwError . show $ (exception :: HttpException)
    Right response -> case eitherDecode (getResponseBody response) of
      Left err -> throwError err
      Right ResultList { results = contracts } -> do
        let nextRange = parseRangeHeader . getResponseHeader "Next-Range" $ response
        return (fromList contracts, nextRange)
