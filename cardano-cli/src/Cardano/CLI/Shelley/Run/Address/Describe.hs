module Cardano.CLI.Shelley.Run.Address.Describe
  ( runAddressDescribe
  ) where

import           Cardano.Prelude hiding (putStrLn)
import           Prelude (putStrLn)

import qualified Data.ByteString.Char8 as BS

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Cardano.Api

import           Cardano.CLI.Ops (CliError (..))
import           Cardano.CLI.Shelley.Parsers
                   (SigningKeyFile (..), VerificationKeyFile (..),
                    AddressCmd(..))

import qualified Cardano.Crypto.Hash.Class as Crypto

import           Data.Char (isAlphaNum)
import qualified Data.ByteString.Base58 as Base58

import qualified Shelley.Spec.Ledger.Keys as Ledger
--TODO: provide address rendering from the main API
--      we should not need to import this directly:
import           Shelley.Spec.Ledger.Address (serialiseAddr)
import qualified Data.ByteString.Base16 as Base16


runAddressDescribe :: ByteString -> ExceptT CliError IO ()
runAddressDescribe addrBS
  | BS.all isAlphaNum addrBS = runAddressDescribeHex addrBS
  | BS.all isBase58Char addrBS = runAddressDescribeBase58 addrBS
  | otherwise = liftIO $ putStrLn $ "Unknown address type: " ++ show addrBS

runAddressDescribeHex :: ByteString -> ExceptT CliError IO ()
runAddressDescribeHex addrBS =
  liftIO $ BS.putStrLn ("Address: " <> addrBS <> " is hex/base16 encoded")

runAddressDescribeBase58 ::ByteString -> ExceptT CliError IO ()
runAddressDescribeBase58 addrBS =
  liftIO $ BS.putStrLn ("Address: " <> addrBS <> " is base58 encoded")


isBase58Char :: Char -> Bool
isBase58Char c = c `elem` BS.unpack (Base58.unAlphabet Base58.bitcoinAlphabet)
