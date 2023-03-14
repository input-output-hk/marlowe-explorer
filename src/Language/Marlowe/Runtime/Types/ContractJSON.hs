{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Marlowe.Runtime.Types.ContractJSON
  ( ContractJSON(..)
  , Link(..)
  , Resource(..)
  , Transaction(..)
  , Transactions(..)
  , getContractJSON
  , getContractTransactions
  ) where

import Data.Aeson ( withObject, (.:?), (.:), FromJSON(parseJSON), eitherDecode, Value )
import Network.HTTP.Simple (parseRequest, getResponseBody, httpLBS, setRequestHeader, setRequestMethod)
import Network.HTTP.Types (urlEncode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (pack, unpack)

import Language.Marlowe.Runtime.Types.Common ( Block, Link(..) )
import Language.Marlowe.Semantics.Types ( Contract(..), State(..) )
import Data.Aeson.Types (Parser)


data ContractJSON = ContractJSON {
    links :: Link,
    resource :: Resource
} deriving (Show, Eq)

instance FromJSON ContractJSON where
  parseJSON :: Value -> Parser ContractJSON
  parseJSON = withObject "JSON" $ \v -> ContractJSON
    <$> (Link <$> (v .: "links" >>= (.: "transactions")))
    <*> v .: "resource"

data Resource = Resource {
    block :: Block,
    contractId :: String,
    currentContract :: Maybe Contract,
    initialContract :: Contract,
    roleTokenMintingPolicyId :: String,
    state :: Maybe State,
    status :: String,
    version :: String
} deriving (Show, Eq)

instance FromJSON Resource where
    parseJSON = withObject "Resource" $ \v -> Resource
        <$> v .: "block"
        <*> v .: "contractId"
        <*> v .:? "currentContract"
        <*> v .: "initialContract"
        <*> v .: "roleTokenMintingPolicyId"
        <*> v .:? "state"
        <*> v .: "status"
        <*> v .: "version"

getContractJSON :: String -> String -> IO (Either String ContractJSON)
getContractJSON endpoint reqContractId = do
    let reqContractIdUrlEncoded = urlEncode False $ encodeUtf8 $ pack reqContractId
    initialRequest <- parseRequest $ endpoint ++ "contracts/" ++ unpack (decodeUtf8 reqContractIdUrlEncoded)
    let request = setRequestMethod "GET" $ setRequestHeader "Accept" ["application/json"] initialRequest
    response <- httpLBS request
    return $ eitherDecode (getResponseBody response)

data Transaction = Transaction
  { txLink :: Link
  , txBlock :: Block
  , txContractId :: String
  , txTransactionId :: String
  } deriving (Show, Eq)

instance FromJSON Transaction where
  parseJSON :: Value -> Parser Transaction
  parseJSON = withObject "Transaction" $ \o -> do
    res <- o .: "resource"
    Transaction
      <$> (Link <$> (o .: "links" >>= (.: "transaction")))
      <*> res .: "block"
      <*> res .: "contractId"
      <*> res .: "transactionId"

newtype Transactions = Transactions [Transaction]
  deriving (Show, Eq)

instance FromJSON Transactions where
  parseJSON :: Value -> Parser Transactions
  parseJSON = withObject "Transactions" $ \o -> do
    Transactions <$> o .: "results"

getContractTransactions :: String -> String -> IO (Either String Transactions)
getContractTransactions endpoint link = do
    initialRequest <- parseRequest $ endpoint ++ link
    let request = setRequestMethod "GET" $ setRequestHeader "Accept" ["application/json"] initialRequest
    response <- httpLBS request
    return $ eitherDecode (getResponseBody response)
