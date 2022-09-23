-- DUPLICATE -- adapted from: cardano-node/src/Cardano/Node/Protocol.hs
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Node.Protocol (
    ProtocolInstantiationError (..)
  , SomeConsensusProtocol (..)
  , mkConsensusProtocol
  ) where

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import           Cardano.Api.Any

import           Cardano.Ledger.Core (Era)
import           Cardano.Ledger.Crypto (StandardCrypto)

import           Cardano.Node.Protocol.Byron
import           Cardano.Node.Protocol.Cardano
import           Cardano.Node.Protocol.Shelley
import           Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))
import           Cardano.Node.Types


------------------------------------------------------------------------------
-- Conversions from configuration into specific protocols and their params
--

mkConsensusProtocol
  :: (Era StandardCrypto)
  => NodeProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ProtocolInstantiationError IO SomeConsensusProtocol
mkConsensusProtocol ncProtocolConfig mProtocolFiles =
    case ncProtocolConfig of

      NodeProtocolConfigurationByron config ->
        firstExceptT ByronProtocolInstantiationError $
          mkSomeConsensusProtocolByron config mProtocolFiles

      NodeProtocolConfigurationShelley config ->
        firstExceptT ShelleyProtocolInstantiationError $
          mkSomeConsensusProtocolShelley config mProtocolFiles

      NodeProtocolConfigurationCardano byronConfig
                                       shelleyConfig
                                       alonzoConfig
                                       hardForkConfig ->
        firstExceptT CardanoProtocolInstantiationError $
          mkSomeConsensusProtocolCardano
            byronConfig
            shelleyConfig
            alonzoConfig
            hardForkConfig
            mProtocolFiles

------------------------------------------------------------------------------
-- Errors
--

data ProtocolInstantiationError =
    ByronProtocolInstantiationError   ByronProtocolInstantiationError
  | ShelleyProtocolInstantiationError ShelleyProtocolInstantiationError
  | CardanoProtocolInstantiationError CardanoProtocolInstantiationError
  deriving Show


instance Error ProtocolInstantiationError where
  displayError (ByronProtocolInstantiationError   err) = displayError err
  displayError (ShelleyProtocolInstantiationError err) = displayError err
  displayError (CardanoProtocolInstantiationError err) = displayError err
