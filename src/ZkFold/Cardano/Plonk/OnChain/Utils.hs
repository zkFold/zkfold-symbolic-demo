module ZkFold.Cardano.Plonk.OnChain.Utils where



import           GHC.ByteOrder                            (ByteOrder (..))
import           PlutusTx                                 (ToData (..))
import           PlutusTx.Builtins
import           PlutusTx.Prelude                         ((.))

import           ZkFold.Cardano.Plonk.OnChain.BLS12_381.F (toF)
import ZkFold.Cardano.Plonk.OnChain.Data

------------------------------------- Utils ------------------------------------

-- convert hash into Zp BLS12_381_Scalar
toInput :: BuiltinByteString -> InputBytes
toInput = InputBytes . toF . byteStringToInteger BigEndian

-- hash transaction data with blake2b_224
dataToBlake :: ToData a => a -> BuiltinByteString
dataToBlake = blake2b_224 . serialiseData . toBuiltinData


