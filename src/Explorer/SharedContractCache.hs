module Explorer.SharedContractCache
  ( ContractListCache
  , newContractList
  , readContractList
  , writeContractList
  )
  where

import Control.Concurrent.MVar ( MVar, modifyMVar_, newMVar, readMVar )
import Language.Marlowe.Runtime.Types.ContractsJSON ( ContractList )


type ContractListCache = MVar ContractList

newContractList :: ContractList -> IO ContractListCache
newContractList = newMVar

writeContractList :: ContractListCache -> ContractList -> IO ()
writeContractList contractListCache contractList =
  modifyMVar_ contractListCache (pure . const contractList)

readContractList :: ContractListCache -> IO ContractList
readContractList = readMVar


