{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Tests consensus-specific crypto operations in relationship with blocks/headers.
module Test.Consensus.Cardano.Crypto (tests) where

import           Cardano.Crypto.VRF (sizeCertVRF)
import qualified Cardano.Crypto.VRF.Praos as Praos
import qualified Cardano.Crypto.VRF.PraosBatchCompat as BatchCompat
import           Data.Function ((&))
import           Ouroboros.Consensus.Cardano.Block (CardanoHeader,
                     StandardCrypto, pattern HeaderAllegra,
                     pattern HeaderAlonzo, pattern HeaderBabbage,
                     pattern HeaderByron, pattern HeaderConway,
                     pattern HeaderMary, pattern HeaderShelley)
import           Ouroboros.Consensus.Shelley.Ledger.Block (Header (..))
import           Ouroboros.Consensus.Shelley.Protocol.Abstract
                     (pTieBreakVRFValue)
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()
import           Ouroboros.Consensus.Protocol.BatchCompatibleCrypto(BatchCompatibleCrypto)
import           Test.Consensus.Cardano.Generators ()
import           Test.QuickCheck (Property, label, property, (===))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup "Cardano Crypto" [
          testProperty "era-dependent VRF" prop_VRFCryptoDependsOnBlockEra
    ]

-- | Check that Babbage and Conway blocks use different VRF crypto.
--
-- This test is based on the following steps:
--
-- 1. generate (forge?) babbage or conway headers
--   - those should contain different VRF proofs (because they are supposed to
--     use different algorithms)
--   - the VRF proof should be invalid for the other era
-- 2. call some header validation functions that's calling VRF certificate check
-- 3. assert that different VRF function is called for each era
--    - the header should be valid
--
-- * why not mock everything? because it does not check that we are
--   implementing things correctly for Conway we would like to test
--   the dispatchign induced by this type, to make clear Conway relies
--   on different crypto primitives
--
-- What needs to change is this type:
--
-- @@
-- type CardanoShelleyEras c =
--   '[ ShelleyBlock (TPraos c) (ShelleyEra c)
--    , ShelleyBlock (TPraos c) (AllegraEra c)
--    , ShelleyBlock (TPraos c) (MaryEra c)
--    , ShelleyBlock (TPraos c) (AlonzoEra c)
--    , ShelleyBlock (Praos c)  (BabbageEra c)
--    , ShelleyBlock (Praos c)  (ConwayEra c)
--    ]
-- @@
--
-- Q: is it enough to test a single specialised crypto function?
--
-- A: yes, because that's a start, but also because using the high
-- level HFBlock even with a single function would be evidence we are
-- doing the dispatching right => we don't test the actual crypto
-- functions, only the "dispatching" logic that requires different
-- instances for different eras.
--
-- Q: Is it enough to test the length of the VRF proof?
--
-- A: Probably
-- not, but it's good enough for now. Should there be a new kind of
-- crypto with different proof/keys/whatever but same length, we would
-- need to beef up the test but that's ok because we always want to
-- have a /red test/ before writing any production code, right?
--
prop_VRFCryptoDependsOnBlockEra :: CardanoHeader StandardCrypto StandardCrypto -> Property
prop_VRFCryptoDependsOnBlockEra = \case
    HeaderShelley ShelleyHeader {shelleyHeaderRaw} ->
      certVRFHasPraosSize shelleyHeaderRaw & label "Shelley"
    HeaderAllegra ShelleyHeader {shelleyHeaderRaw} ->
      certVRFHasPraosSize shelleyHeaderRaw & label "Allegra"
    HeaderMary ShelleyHeader {shelleyHeaderRaw} ->
      certVRFHasPraosSize shelleyHeaderRaw & label "Mary"
    HeaderAlonzo ShelleyHeader {shelleyHeaderRaw} ->
      certVRFHasPraosSize shelleyHeaderRaw & label "Alonzo"
    HeaderBabbage ShelleyHeader {shelleyHeaderRaw} ->
      certVRFHasPraosSize shelleyHeaderRaw & label "Babbage"
    HeaderConway ShelleyHeader {shelleyHeaderRaw} ->
      certVRFHasBatchCompatSize shelleyHeaderRaw & label "Conway"
    HeaderByron _ -> property True & label "Byron"
  where
    certVRFHasPraosSize hdrRaw = sizeCertVRF (pTieBreakVRFValue hdrRaw) === fromIntegral Praos.certSizeVRF
    certVRFHasBatchCompatSize hdrRaw = sizeCertVRF (pTieBreakVRFValue hdrRaw) === fromIntegral BatchCompat.certSizeVRF
