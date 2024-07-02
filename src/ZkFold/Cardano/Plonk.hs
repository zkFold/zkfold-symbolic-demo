{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.Plonk where

import           GHC.ByteOrder                             (ByteOrder (..))
import           PlutusTx.Builtins
import           PlutusTx.Prelude                          (Bool (..), ($), (.), (<>), (&&), Eq (..))
import           Prelude                                   (undefined)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Protocol.NonInteractiveProof  (NonInteractiveProof (..))
import           ZkFold.Cardano.Plonk.OnChain.BLS12_381.F  (F (..), powTwo)
import           ZkFold.Cardano.Plonk.OnChain.BLS12_381.G1 (mul)
import           ZkFold.Cardano.Plonk.OnChain.Data         (InputBytes (..), ProofBytes (..), SetupBytes (..))

data PlonkPlutus

instance NonInteractiveProof PlonkPlutus where
    type Transcript PlonkPlutus  = BuiltinByteString
    type SetupProve PlonkPlutus  = ()
    type SetupVerify PlonkPlutus = SetupBytes
    type Witness PlonkPlutus     = ()
    type Input PlonkPlutus       = InputBytes
    type Proof PlonkPlutus       = ProofBytes

    setupProve :: PlonkPlutus -> SetupProve PlonkPlutus
    setupProve = undefined

    setupVerify :: PlonkPlutus -> SetupVerify PlonkPlutus
    setupVerify = undefined

    prove :: SetupProve PlonkPlutus -> Witness PlonkPlutus -> (Input PlonkPlutus, Proof PlonkPlutus)
    prove = undefined

    {-# INLINABLE verify #-}
    verify :: SetupVerify PlonkPlutus -> Input PlonkPlutus -> Proof PlonkPlutus -> Bool
    verify SetupBytes{..} InputBytes{..} ProofBytes{..} =
        let -- uncompress Setup G1 elements
            g0    = bls12_381_G1_uncompress g0'
            h0    = bls12_381_G2_uncompress h0'
            h1    = bls12_381_G2_uncompress h1'
            cmQl  = bls12_381_G1_uncompress cmQl'
            cmQr  = bls12_381_G1_uncompress cmQr'
            cmQo  = bls12_381_G1_uncompress cmQo'
            cmQm  = bls12_381_G1_uncompress cmQm'
            cmQc  = bls12_381_G1_uncompress cmQc'
            cmS1  = bls12_381_G1_uncompress cmS1'
            cmS2  = bls12_381_G1_uncompress cmS2'
            cmS3  = bls12_381_G1_uncompress cmS3'

            -- uncompress Proof G1 and wrap Integer to F
            cmA    = bls12_381_G1_uncompress cmA'
            cmB    = bls12_381_G1_uncompress cmB'
            cmC    = bls12_381_G1_uncompress cmC'
            cmZ    = bls12_381_G1_uncompress cmZ'
            cmT1   = bls12_381_G1_uncompress cmT1'
            cmT2   = bls12_381_G1_uncompress cmT2'
            cmT3   = bls12_381_G1_uncompress cmT3'
            proof1 = bls12_381_G1_uncompress proof1'
            proof2 = bls12_381_G1_uncompress proof2'
            a_xi   = F a_xi'
            b_xi   = F b_xi'
            c_xi   = F c_xi'
            s1_xi  = F s1_xi'
            s2_xi  = F s2_xi'
            z_xi   = F z_xi'

            -- create beta, gamma, alpha, xi, v, u from Transcript
            t0 = consByteString 0 $ cmA' <> cmB' <> cmC'
            beta = F . byteStringToInteger LittleEndian . blake2b_224 $ t0

            t1 = consByteString 0 t0
            gamma = F . byteStringToInteger LittleEndian . blake2b_224 $ t1

            t2 = consByteString 0 $ t1 <> cmZ'
            alpha = F . byteStringToInteger LittleEndian . blake2b_224 $ t2

            t3 = consByteString 0 $ t2 <> cmT1' <> cmT2' <> cmT3'
            xi = F . byteStringToInteger LittleEndian . blake2b_224 $ t3

            t4 = consByteString 0 $ t3
                <> integerToByteString LittleEndian 32 a_xi'
                <> integerToByteString LittleEndian 32 b_xi'
                <> integerToByteString LittleEndian 32 c_xi'
                <> integerToByteString LittleEndian 32 s1_xi'
                <> integerToByteString LittleEndian 32 s2_xi'
                <> integerToByteString LittleEndian 32 z_xi'
            v = F . byteStringToInteger LittleEndian . blake2b_224 $ t4

            u = F . byteStringToInteger LittleEndian . blake2b_224 $ t4 <> proof1' <> proof2'

            -- common varibles for r0, d, f, e

            xi_n = xi `powTwo` pow
            xi_m_one = xi_n - one

            lagrange1_xi = omega * xi_m_one * lagsInv

            alphaSquare = alpha * alpha
            alphaEvalZOmega = alpha * z_xi
            betaZeta = beta * xi

            a_xi_gamma = a_xi + gamma
            b_xi_gamma = b_xi + gamma
            c_xi_gamma = c_xi + gamma

            beta_s1_xi = beta * s1_xi
            beta_s2_xi = beta * s2_xi

            gamma_beta_a_s1 = a_xi_gamma + beta_s1_xi
            gamma_beta_b_s2 = b_xi_gamma + beta_s2_xi

            -- final calculations
            r0 =
                  negate pubInput * lagrange1_xi
                - alphaSquare * lagrange1_xi
                - alphaEvalZOmega
                    * gamma_beta_a_s1
                    * gamma_beta_b_s2
                    * c_xi_gamma

            d  =
                  mul (a_xi * b_xi) cmQm
                + mul a_xi cmQl
                + mul b_xi cmQr
                + mul c_xi cmQo
                + cmQc
                + mul (
                          alpha
                        * (a_xi_gamma + betaZeta)
                        * (b_xi_gamma + betaZeta * k1)
                        * (c_xi_gamma + betaZeta * k2)
                    +     alphaSquare * lagrange1_xi
                    +     u
                    ) cmZ
                - mul (
                      alphaEvalZOmega
                    * beta
                    * gamma_beta_a_s1
                    * gamma_beta_b_s2
                    ) cmS3
                - mul xi_m_one (cmT1 + xi_n `mul` cmT2 + (xi_n * xi_n) `mul` cmT3)

            f  = d
                + v `mul` (cmA
                + v `mul` (cmB
                + v `mul` (cmC
                + v `mul` (cmS1
                + v `mul` cmS2))))

            e  = (
                negate r0
                + v * (a_xi
                + v * (b_xi
                + v * (c_xi
                + v * (s1_xi
                + v * s2_xi))))
                + u * z_xi
                ) `mul` g0 -- bls12_381_G1_generator ?

            p1 = bls12_381_millerLoop (xi `mul` proof1 + (u * xi * omega) `mul` proof2 + f - e) h0 -- bls12_381_G2_generator ?
            p2 = bls12_381_millerLoop (proof1 + u `mul` proof2) h1

        in bls12_381_finalVerify p1 p2 && lagsInv * F n * (xi - omega) == one
