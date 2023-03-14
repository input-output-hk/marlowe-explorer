{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Marlowe.Runtime.Types.Common
  ( Block(..)
  , Link(..)
  ) where

import Data.Aeson ( withObject, (.:), FromJSON(parseJSON) )
import Data.Aeson.Types (Value, Parser)

-- Many of the resources we get back from Marlowe Runtime have a single link of
-- some named kind, but the name varies ("contract" or "transaction"). This
-- type is for holding onto that link. The JSON path will be expressed in the
-- containing type's FromJSON intance.
newtype Link = Link { linkUrl :: String }
  deriving (Show, Eq)

data Block = Block
  { blockHeaderHash :: String
  , blockNo :: Integer
  , slotNo :: Integer
  }
  deriving (Show, Eq)

instance FromJSON Block where
  parseJSON :: Value -> Parser Block
  parseJSON = withObject "Block" $ \v -> Block
    <$> v .: "blockHeaderHash"
    <*> v .: "blockNo"
    <*> v .: "slotNo"
