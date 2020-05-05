{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--TODO: eliminate partial conversions:
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.Api.Types
  (
    -- * Common types across all eras.
    Address (..)
  , Network (..)
  , SigningKey (..)
  , VerificationKey (..)
  , Certificate (..)
  , TxSigned (..)
  , TxUnsigned (..)
  , TxWitness (..)
  , TxIn (..)
  , TxId (..)
  , TxIx
  , TxOut (..)
  , SlotNo (..)
  , Lovelace (..)

    -- * Era-specific type aliases and conversions
    -- ** Byron
  , ByronVerificationKey
  , ByronSigningKey
  , ByronAddress
  , ByronTxBody
  , ByronTx
  , ByronTxId
  , ByronTxIn
  , ByronTxOut
  , ByronWitness
  , toByronTxIn
  , toByronTxOut
  , toByronLovelace

    -- ** Shelley
  , ShelleyCertificate
  , ShelleyCoin
  , ShelleyGenesisDelegationCertificate
  , ShelleyGenesisVerificationKey
  , ShelleyGenesisVerificationHash
  , ShelleyMIRCertificate
  , ShelleyMIRMap
  , ShelleyStakePoolMargin
  , ShelleyStakePoolCertificate
  , ShelleyStakePoolMetaData
  , ShelleyStakePoolOwners
  , ShelleyStakePoolRelay
  , ShelleyVerificationKey
  , ShelleyVerificationKeyHash
  , ShelleySigningKey
  , ShelleyVRFVerificationKeyHash
  , ShelleyAddress
  , ShelleyCredential
  , ShelleyDelegationCertificate
  , ShelleyRewardAccount
  , ShelleyTxBody
  , ShelleyTx
  , ShelleyTxId
  , ShelleyTxIn
  , ShelleyTxOut
  , ShelleyWitnessVKey
  , toShelleyTxIn
  , toShelleyTxOut
  , toShelleyLovelace
  ) where

import           Cardano.Prelude

import           Data.Vector (Vector)

import           Cardano.Config.Orphanage ()

import           Cardano.Slotting.Slot (SlotNo (..))

import qualified Cardano.Crypto.Hash.Class   as Crypto
import qualified Cardano.Crypto.Hash.Blake2b as Crypto

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO   as Byron
import qualified Cardano.Crypto       as Byron

import qualified Cardano.Crypto.Hash                         as HASH
import qualified Cardano.Crypto.VRF                          as VRF
import qualified Ouroboros.Consensus.Shelley.Protocol.Crypto as Shelley
import qualified Shelley.Spec.Ledger.BaseTypes               as Shelley
import qualified Shelley.Spec.Ledger.Coin                    as Shelley
import qualified Shelley.Spec.Ledger.Keys                    as Shelley
import qualified Shelley.Spec.Ledger.TxData                  as Shelley
import qualified Shelley.Spec.Ledger.Tx                      as Shelley


type ByronVerificationKey = Byron.VerificationKey
type ByronSigningKey      = Byron.SigningKey
type ByronAddress         = Byron.Address
type ByronTxIn            = Byron.TxIn
type ByronTxOut           = Byron.TxOut
type ByronTxBody          = Byron.Tx
type ByronTx              = Byron.TxAux
type ByronTxId            = Byron.TxId
type ByronWitness         = Byron.TxInWitness

type ShelleyCoin                         = Shelley.Coin
type ShelleyGenesisVerificationKey       = Shelley.VKeyGenesis   Shelley.TPraosStandardCrypto
type ShelleyGenesisVerificationHash      = Shelley.GenKeyHash     Shelley.TPraosStandardCrypto
type ShelleyVerificationKey              = Shelley.VKey           Shelley.TPraosStandardCrypto
type ShelleyVerificationKeyHash          = Shelley.KeyHash        Shelley.TPraosStandardCrypto
type ShelleySigningKey                   = Shelley.SKey           Shelley.TPraosStandardCrypto
type ShelleyVRFVerificationKeyHash       = Shelley.Hash           HASH.Blake2b_256 (Shelley.VerKeyVRF VRF.SimpleVRF)
type ShelleyAddress                      = Shelley.Addr           Shelley.TPraosStandardCrypto
type ShelleyTxIn                         = Shelley.TxIn           Shelley.TPraosStandardCrypto
type ShelleyTxOut                        = Shelley.TxOut          Shelley.TPraosStandardCrypto
type ShelleyTxBody                       = Shelley.TxBody         Shelley.TPraosStandardCrypto
type ShelleyTx                           = Shelley.Tx             Shelley.TPraosStandardCrypto
type ShelleyTxId                         = Shelley.TxId           Shelley.TPraosStandardCrypto
type ShelleyWitnessVKey                  = Shelley.WitVKey        Shelley.TPraosStandardCrypto
type ShelleyCertificate                  = Shelley.DCert          Shelley.TPraosStandardCrypto
type ShelleyDelegationCertificate        = Shelley.DelegCert      Shelley.TPraosStandardCrypto
type ShelleyGenesisDelegationCertificate = Shelley.GenesisDelegate Shelley.TPraosStandardCrypto
type ShelleyMIRCertificate               = Shelley.MIRCert Shelley.TPraosStandardCrypto
type ShelleyMIRMap                       = Map (Shelley.Credential Shelley.TPraosStandardCrypto) Shelley.Coin
type ShelleyStakePoolCertificate         = Shelley.PoolCert       Shelley.TPraosStandardCrypto
type ShelleyStakePoolOwners              = Set                    (Shelley.KeyHash Shelley.TPraosStandardCrypto)
type ShelleyStakePoolMargin              = Shelley.UnitInterval
type ShelleyStakePoolMetaData            = Shelley.PoolMetaData
type ShelleyStakePoolRelay               = Shelley.StakePoolRelay
type ShelleyRewardAccount                = Shelley.RewardAcnt     Shelley.TPraosStandardCrypto
type ShelleyCredential                   = Shelley.Credential     Shelley.TPraosStandardCrypto
{-
type ShelleyVerificationKey        = Shelley.VKey   Shelley.TPraosStandardCrypto
type ShelleyVerificationKeyHash    = Shelley.DiscKeyHash 'Regular Shelley.TPraosStandardCrypto
type ShelleySigningKey             = Shelley.SKey   Shelley.TPraosStandardCrypto
type ShelleyVRFVerificationKeyHash = Shelley.Hash HASH.Blake2b_256 (Shelley.VerKeyVRF VRF.SimpleVRF)
type ShelleyAddress         = Shelley.Addr   Shelley.TPraosStandardCrypto
type ShelleyTxIn            = Shelley.TxIn   Shelley.TPraosStandardCrypto
type ShelleyTxOut           = Shelley.TxOut  Shelley.TPraosStandardCrypto
type ShelleyTxBody          = Shelley.TxBody Shelley.TPraosStandardCrypto
type ShelleyTx              = Shelley.Tx     Shelley.TPraosStandardCrypto
type ShelleyTxId            = Shelley.TxId   Shelley.TPraosStandardCrypto
type ShelleyWitnessVKey     = Shelley.WitVKey Shelley.TPraosStandardCrypto
type ShelleyCertificate                  = Shelley.DCert Shelley.TPraosStandardCrypto
type ShelleyDelegationCertificate        = Shelley.DelegCert Shelley.TPraosStandardCrypto
type ShelleyStakePoolCertificate         = Shelley.PoolCert Shelley.TPraosStandardCrypto
type ShelleyStakePoolOwners              = Set (Shelley.KeyHash Shelley.TPraosStandardCrypto)
type ShelleyStakePoolMargin              = Shelley.UnitInterval
type ShelleyRewardAccount                = Shelley.RewardAcnt Shelley.TPraosStandardCrypto
type ShelleyCredential                   = Shelley.Credential Shelley.TPraosStandardCrypto
-}
-- The 'Address' data type in 'cardano-sl' is a design train wreck.
-- We need something that is compatible and discard as much of the insanity as possible.
data Address
  = AddressByron !ByronAddress
  | AddressShelley !ShelleyAddress
  deriving (Eq, Generic , NFData, Show)  -- Byron.Address' needs NFData
  deriving NoUnexpectedThunks via UseIsNormalForm Address


data Certificate
  = ShelleyDelegationCertificate !ShelleyCertificate
  | ShelleyStakePoolCertificate !ShelleyCertificate
  | ShelleyGenesisDelegationCertificate !ShelleyCertificate
  | ShelleyMIRCertificate !ShelleyCertificate
  deriving (Eq, Show)



-- | The combination of a verification key and a signing key.
--
-- Verification keys are also commonly known as \"public keys\".
--
-- Signing keys are also commonly known as \"private keys\" or \"secret keys\".
--
data SigningKey
  -- The Byron key pair use newtype wrappers around 'XPriv'/'Xpub' keys.
  -- An 'XPub' is 32 bytes of public key and 32 bytes of Chaincode which is used in the
  -- Byron address derivation scheme.
  = SigningKeyByron   !ByronSigningKey
  | SigningKeyShelley !ShelleySigningKey
  deriving (Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

-- | A verification key for use in addresses (payment and stake).
--
-- Verification keys are also commonly known as \"public keys\".
--
data VerificationKey
  = VerificationKeyByron !ByronVerificationKey
  | VerificationKeyShelley !ShelleyVerificationKey
  deriving (Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

-- The cardano-sl codebase (and cardano-ledger) has something a little like
-- this (actually isomorphic with Maybe):
data Network
  = Mainnet
  | Testnet !Byron.ProtocolMagicId
  deriving (Eq, Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

data TxIn = TxIn !TxId !TxIx
  deriving (Eq, Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

newtype TxId = TxId (Crypto.Hash Crypto.Blake2b_256 ())
  deriving (Eq, Generic, Show)
  deriving newtype (NFData, NoUnexpectedThunks)

type TxIx = Word

data TxOut = TxOut !Address !Lovelace
  deriving (Eq, Generic, NFData, Show)
  deriving anyclass NoUnexpectedThunks

newtype Lovelace
  = Lovelace { unLoveLace :: Integer }
  deriving (Eq, Generic)
  deriving newtype (Read, Show)
  deriving anyclass (NFData, NoUnexpectedThunks)

toByronTxIn  :: TxIn  -> ByronTxIn
toByronTxIn (TxIn txid txix) =
    Byron.TxInUtxo (toByronTxId txid) (fromIntegral txix)

toByronTxOut :: TxOut -> ByronTxOut
toByronTxOut (TxOut (AddressByron addr) value) =
    Byron.TxOut addr (toByronLovelace value)
toByronTxOut (TxOut (AddressShelley _) _) =
    panic "TODO: toByronTxOut AddressShelley"

toShelleyTxIn :: TxIn -> ShelleyTxIn
toShelleyTxIn (TxIn txid txix) =
    Shelley.TxIn (toShelleyTxId txid) (fromIntegral txix)

toShelleyTxOut :: TxOut -> ShelleyTxOut
toShelleyTxOut (TxOut (AddressShelley addr) value) =
    Shelley.TxOut addr (toShelleyLovelace value)
toShelleyTxOut (TxOut (AddressByron _) _) =
    panic "TODO: toShelleyTxOut convert byron address to Shelley bootstrap address"

toByronTxId :: TxId -> ByronTxId
toByronTxId (TxId (Crypto.UnsafeHash h)) =
    Byron.unsafeHashFromBytes h

toShelleyTxId :: TxId -> ShelleyTxId
toShelleyTxId (TxId (Crypto.UnsafeHash h)) =
    Shelley.TxId (Crypto.UnsafeHash h)

toByronLovelace :: Lovelace -> Byron.Lovelace
toByronLovelace (Lovelace x) = x' where Right x' = Byron.integerToLovelace x
                  --TODO: deal with partial conversion

toShelleyLovelace :: Lovelace -> Shelley.Coin
toShelleyLovelace (Lovelace l) = Shelley.Coin l


data TxSigned
  = TxSignedByron !ByronTxBody !ByteString !(Byron.Hash ByronTxBody) !(Vector Byron.TxInWitness)
  | TxSignedShelley !ShelleyTx
  deriving (Eq, Generic, {-NFData, TODO -} Show)
  deriving NoUnexpectedThunks via UseIsNormalForm TxSigned

data TxUnsigned
  = TxUnsignedByron !ByronTxBody !ByteString !(Byron.Hash ByronTxBody)
  | TxUnsignedShelley !ShelleyTxBody
  deriving (Eq, Generic, {-NFData, TODO -} Show)
  deriving NoUnexpectedThunks via UseIsNormalForm TxUnsigned

data TxWitness
  = TxWitByron   !ByronWitness
  | TxWitShelley !ShelleyWitnessVKey
--    | TxWitShelleyScript !ShelleyWitnessScript
  deriving (Generic{-, NFData TODO -})
  deriving NoUnexpectedThunks via UseIsNormalForm TxWitness
