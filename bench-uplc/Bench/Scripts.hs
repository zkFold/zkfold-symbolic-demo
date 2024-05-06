{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Bench.Scripts (compiledSymbolicVerifier, compiledPlonkVerifier, compiledPlonkVerify) where

import           PlutusLedgerApi.V3                       (ScriptContext (..))
import           PlutusTx                                 (CompiledCode)
import           PlutusTx.Prelude                         (Bool)
import           PlutusTx.TH                              (compile)

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.ScriptsVerifier           (plonkVerifier, symbolicVerifier)

compiledSymbolicVerifier :: CompiledCode (Setup PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> ScriptContext -> Bool)
compiledSymbolicVerifier = $$(compile [|| symbolicVerifier ||])

compiledPlonkVerifier :: CompiledCode (Setup PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> ScriptContext -> Bool)
compiledPlonkVerifier = $$(compile [|| plonkVerifier ||])

compiledPlonkVerify :: CompiledCode (Setup PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> Bool)
compiledPlonkVerify = $$(compile [|| verify @PlonkPlutus ||])