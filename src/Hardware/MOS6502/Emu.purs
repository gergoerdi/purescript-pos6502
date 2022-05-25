module Hardware.MOS6502.Emu where

import Prelude
import Data.Word
import Data.Integral (fromIntegral)

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Class
import Control.Monad.Reader

type Addr = Word16

class (MonadEffect m) <= MonadMachine m where
    readMem :: Addr -> m Word8
    writeMem :: Addr -> Word8 -> m Unit

instance (MonadMachine m) => MonadMachine (ReaderT r m) where
    readMem = lift <<< readMem
    writeMem addr = lift <<< writeMem addr

type CPU = 
    { regA :: Ref Word8
    , regX :: Ref Word8
    , regY :: Ref Word8
    , status :: Ref Word8
    , sp :: Ref Word8
    , pc :: Ref Addr
    }

new :: Addr -> Effect CPU
new pc0 = do
    regA <- Ref.new <<< fromIntegral $ 0x00
    regX <- Ref.new <<< fromIntegral $ 0x00
    regY <- Ref.new <<< fromIntegral $ 0x00
    status <- Ref.new <<< fromIntegral $ 0x00
    sp <- Ref.new <<< fromIntegral $ 0xff
    pc <- Ref.new pc0
    pure { regA: regA, regX: regX, regY: regY, status: status, sp: sp, pc: pc }

fetch :: forall m. (MonadMachine m) => ReaderT CPU m Word8
fetch = do
    pc <- asks \obj -> obj.pc
    addr <- liftEffect $ Ref.read pc
    liftEffect $ flip Ref.write pc $ addr + fromIntegral 1
    readMem addr
