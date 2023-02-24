{-# LANGUAGE OverloadedStrings #-}

module Language.Marlowe.Runtime.Types.ContractsJSON
  ( ContractList(..)
  , ContractInList(..)
  , Resource(..)
  , getContracts
  )
  where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson ( withObject, (.:), FromJSON(parseJSON), eitherDecode )
import Data.ByteString ( ByteString )
import Data.Foldable ( toList )  -- Used in this module as the counterpart to Seq.fromList
import Data.List ( foldl' )
import qualified Data.Sequence as Seq
import Data.Sequence ( Seq, (><) )
import Network.HTTP.Simple ( Request, parseRequest, getResponseBody, httpLBS,
  getResponseHeader, setRequestHeader, setRequestMethod )

import Language.Marlowe.Runtime.Types.Common ( Block, Link(..) )


data Range
  = Start
  | Next ByteString
  | Done
  deriving (Eq, Show)

newtype ContractList = ContractList [ContractInList]
  deriving (Eq, Show)

instance FromJSON ContractList where
  parseJSON = withObject "ContractList" $ \o -> do
    ContractList <$> o .: "results"

data ContractInList = ContractInList
  { cilLink :: Link
  , cilResource :: Resource
  }
  deriving (Eq, Show)

instance FromJSON ContractInList where
  parseJSON = withObject "ContractInList" $ \o -> ContractInList
    <$> (Link <$> (o .: "links" >>= (.: "contract")))
    <*> o .: "resource"

data Resource = Resource
  { resContractId :: String
  , resBlock :: Block
  }
  deriving (Eq, Show)

instance FromJSON Resource where
  parseJSON = withObject "Resource" $ \o -> Resource
    <$> o .: "contractId"
    <*> o .: "block"


type GetContracts a = ReaderT String (ExceptT String IO) a

runGetContracts :: String -> GetContracts a -> IO (Either String a)
runGetContracts env ev = runExceptT $ runReaderT ev env

getContracts :: String -> IO (Either String ContractList)
getContracts endpoint = do
  eresult <- runGetContracts endpoint $ getContracts' (Seq.empty, Start)
  return $ (Right . ContractList . toList . fst) =<< eresult


getContracts' :: (Seq ContractInList, Range) -> GetContracts (Seq ContractInList, Range)

getContracts' t@(_acc, Done) = return t

getContracts' (acc, range) = do
  (nextContracts, nextRange) <- contractsRESTCall range
  getContracts' (acc >< nextContracts, nextRange)


setRangeHeader :: Range -> Request -> Request
setRangeHeader (Next bs) = setRequestHeader "Range" [bs]
setRangeHeader _ = id

parseRangeHeader :: [ByteString] -> Range
parseRangeHeader [bs] = Next bs
parseRangeHeader _ = Done

contractsRESTCall :: Range -> GetContracts (Seq ContractInList, Range)
contractsRESTCall range = do
  endpoint <- ask
  initialRequest <- parseRequest $ endpoint <> "contracts"
  let request = foldl' (flip id) initialRequest
        [ setRequestMethod "GET"
        , setRequestHeader "Accept" ["application/json"]
        , setRangeHeader range
        ]
  response <- liftIO $ httpLBS request
  case eitherDecode (getResponseBody response) of
    Left err -> throwError err
    Right (ContractList contracts) -> do
      let nextRange = parseRangeHeader . getResponseHeader "Next-Range" $ response
      return (Seq.fromList contracts, nextRange)
