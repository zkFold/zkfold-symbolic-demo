{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.Scripts.PlonkVerifier where

import           PlutusLedgerApi.V1.Value                 (Value (..))
import           PlutusLedgerApi.V3                       (ScriptContext (..), TokenName (..), TxInfo (..))
import           PlutusLedgerApi.V3.Contexts              (ownCurrencySymbol)
import qualified PlutusTx.AssocMap                        as AssocMap
import           PlutusTx.Prelude                         (Bool (..), Maybe (..), Ord (..), ($), (||))

import           ZkFold.Base.Protocol.NonInteractiveProof (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk                     (PlonkPlutus)
import           ZkFold.Cardano.Plonk.OnChain             (ProofBytes, SetupBytes, toInput)

-- | Plutus script (minting policy) for verifying computations on-chain.
--
-- The token is minted if and only if the Plonk `proof` is valid for the `computation` on the `input` derived from the token name.
-- The computation is encoded into the token's currency symbol (aka policyID).
{-# INLINABLE plonkVerifier #-}
plonkVerifier :: SetupBytes -> ProofBytes -> ScriptContext -> Bool
plonkVerifier computation proof ctx =
    conditionBurning || conditionVerifying
    where
        mints              = getValue $ txInfoMint $ scriptContextTxInfo ctx

        -- Finding own tokens
        Just m             = AssocMap.lookup (ownCurrencySymbol ctx) mints
        [(TokenName t, n)] = AssocMap.toList m

        -- Computing public input from the token name
        input              = toInput t

        -- Burning already minted tokens
        conditionBurning   = n < 0

        -- Verifying the Plonk `proof` for the `computation` on `input`
        conditionVerifying = verify @PlonkPlutus computation input proof
