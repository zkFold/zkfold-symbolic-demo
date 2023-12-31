{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module ZkFold.Symbolic.Verifier.Internal where

import           PlutusTx                                 (makeLift, makeIsDataIndexed)
import           PlutusTx.Builtins                        
import           PlutusTx.Prelude                         hiding (fromInteger)
import           Prelude                                  (Num (fromInteger))
import qualified Prelude                                  as Haskell

import qualified ZkFold.Base.Algebra.Basic.Class          as ZkFold
import           ZkFold.Base.Algebra.Basic.Field          (Ext2 (..), fromZp, toZp)
import           ZkFold.Base.Algebra.EllipticCurve.Class  (Point(..))
import           ZkFold.Base.Protocol.NonInteractiveProof (ToTranscript (..), FromTranscript (..))
import qualified ZkFold.Base.Protocol.ARK.Plonk           as Plonk

-- TODO: separate on-chain and off-chain code

---------------------------------- F --------------------------------------

bls12_381_field_prime :: Integer
bls12_381_field_prime = 52435875175126190479447740508185965837690552500527637822603658699938581184513

newtype F = F { toF :: Integer }
    deriving (Haskell.Show)
makeLift ''F
makeIsDataIndexed ''F [('F,0)]

instance Eq F where
    {-# INLINABLE (==) #-}
    (F a) == (F b) = a == b

instance ZkFold.AdditiveSemigroup F where
    {-# INLINABLE (+) #-}
    (F a) + (F b) = F $ (a + b) `modulo` bls12_381_field_prime

instance ZkFold.AdditiveMonoid F where
    {-# INLINABLE zero #-}
    zero = F 0

instance ZkFold.AdditiveGroup F where
    {-# INLINABLE (-) #-}
    (F a) - (F b) = F $ (a - b) `modulo` bls12_381_field_prime

instance ZkFold.MultiplicativeSemigroup F where
    {-# INLINABLE (*) #-}
    (F a) * (F b) = F $ (a * b) `modulo` bls12_381_field_prime

instance ZkFold.MultiplicativeMonoid F where
    {-# INLINABLE one #-}
    one = F 1

{-# INLINABLE powMod #-}
powMod :: F -> Integer -> F
powMod b e
    | e < 0     = ZkFold.zero
    | e == 0    = ZkFold.one
    | even e    = powMod (b ZkFold.* b) (e `divide` 2)
    | otherwise = b ZkFold.* powMod (b ZkFold.* b) ((e - 1) `divide` 2)

instance ZkFold.MultiplicativeGroup F where
    {-# INLINABLE invert #-}
    invert a = powMod a (bls12_381_field_prime - 2)

    {-# INLINABLE (/) #-}
    a / b = a ZkFold.* ZkFold.invert b

instance Num F where
    {-# INLINABLE (+) #-}
    (+) = (ZkFold.+)

    {-# INLINABLE (-) #-}
    (-) = (ZkFold.-)

    {-# INLINABLE (*) #-}
    (*) = (ZkFold.*)

    {-# INLINABLE negate #-}
    negate = ZkFold.negate

    {-# INLINABLE abs #-}
    abs = id

    {-# INLINABLE signum #-}
    signum = const ZkFold.one

    {-# INLINABLE fromInteger #-}
    fromInteger = F . (`modulo` bls12_381_field_prime)

---------------------------------- G1 -------------------------------------

type G1 = BuiltinBLS12_381_G1_Element

instance ZkFold.AdditiveSemigroup  BuiltinBLS12_381_G1_Element where
    {-# INLINABLE (+) #-}
    (+) = bls12_381_G1_add

instance ZkFold.AdditiveMonoid BuiltinBLS12_381_G1_Element where
    {-# INLINABLE zero #-}
    zero = bls12_381_G1_uncompress bls12_381_G1_compressed_zero

instance ZkFold.AdditiveGroup BuiltinBLS12_381_G1_Element where
    {-# INLINABLE (-) #-}
    g - h = bls12_381_G1_add g (bls12_381_G1_neg h)

mul :: F -> BuiltinBLS12_381_G1_Element -> BuiltinBLS12_381_G1_Element
mul (F a) = bls12_381_G1_scalarMul a

---------------------------------- G2 -------------------------------------

type G2 = BuiltinBLS12_381_G2_Element

instance ZkFold.AdditiveSemigroup  BuiltinBLS12_381_G2_Element where
    {-# INLINABLE (+) #-}
    (+) = bls12_381_G2_add

instance ZkFold.AdditiveMonoid BuiltinBLS12_381_G2_Element where
    {-# INLINABLE zero #-}
    zero = bls12_381_G2_uncompress bls12_381_G2_compressed_zero

instance ZkFold.AdditiveGroup BuiltinBLS12_381_G2_Element where
    {-# INLINABLE (-) #-}
    g - h = bls12_381_G2_add g (bls12_381_G2_neg h)

-------------------------- Conversions ------------------------------------

convertF :: Plonk.F -> F
convertF = F . fromZp

-- See CIP-0381 for the conversion specification
convertG1 :: Plonk.G1 -> G1
convertG1 Inf = bls12_381_G1_uncompress bls12_381_G1_compressed_zero
convertG1 (Point x y) = bls12_381_G1_uncompress bs
    where
        bsX = builtinIntegerToByteString True 48 $ fromZp x
        b   = indexByteString bsX 0
        b'  = b + 128 + 32 * (if y Haskell.> ZkFold.negate y then 1 else 0)
        bs  = consByteString b' $ sliceByteString 1 47 bsX

-- See CIP-0381 for the conversion specification
convertG2 :: Plonk.G2 -> G2
convertG2 Inf = bls12_381_G2_uncompress bls12_381_G2_compressed_zero
convertG2 (Point x y) = bls12_381_G2_uncompress bs
    where
        f (Ext2 a0 a1) = builtinIntegerToByteString True 48 (fromZp a1) <> builtinIntegerToByteString True 48 (fromZp a0)
        bsX  = f x
        bsY  = f y
        bsY' = f $ ZkFold.negate y
        b   = indexByteString bsX 0
        b'  = b + 128 + 32 * (if bsY `greaterThanByteString` bsY' then 1 else 0)
        bs  = consByteString b' $ sliceByteString 1 95 bsX

-------------------------- Transcript -------------------------------------

type Transcript = BuiltinByteString

instance ToTranscript BuiltinByteString F where
    {-# INLINABLE toTranscript #-}
    toTranscript (F a) = builtinIntegerToByteString True 0 a

instance ToTranscript BuiltinByteString Plonk.F where
    toTranscript = toTranscript . F . fromZp

{-# INLINABLE transcriptF #-}
transcriptF :: Transcript -> F -> Transcript
transcriptF ts a = ts <> toTranscript a

instance ToTranscript BuiltinByteString G1 where
    {-# INLINABLE toTranscript #-}
    toTranscript = bls12_381_G1_compress

instance ToTranscript BuiltinByteString Plonk.G1 where
    toTranscript = toTranscript . convertG1

{-# INLINABLE transcriptG1 #-}
transcriptG1 :: Transcript -> G1 -> Transcript
transcriptG1 ts g = ts <> toTranscript g

instance FromTranscript BuiltinByteString F where
    {-# INLINABLE newTranscript #-}
    newTranscript = consByteString 0

    {-# INLINABLE fromTranscript #-}
    fromTranscript = fromInteger . builtinByteStringToInteger True . takeByteString 31 . blake2b_256

instance FromTranscript BuiltinByteString Plonk.F where
    newTranscript = newTranscript @BuiltinByteString @F

    fromTranscript = toZp . toF . fromTranscript @BuiltinByteString @F

{-# INLINABLE challenge #-}
challenge :: Transcript -> (F, Transcript)
challenge ts =
    let ts' = newTranscript @BuiltinByteString @F ts
    in (fromTranscript ts', ts')