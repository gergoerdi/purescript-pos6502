module Hardware.MOS6502.Emu where

import Prelude
import Data.Word
import Data.Integral (fromIntegral)
import Data.UInt (fromInt)
import Data.Shift

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
    pc <- asks _.pc
    addr <- liftEffect $ Ref.read pc
    liftEffect $ Ref.write (addr + fromIntegral 1) pc
    readMem addr

toAddr :: Word8 -> Word8 -> Addr
toAddr lo hi = (fromIntegral hi `shl` fromInt 8)  .|. fromIntegral lo

fetchAddr :: forall m. (MonadMachine m) => ReaderT CPU m Addr
fetchAddr = toAddr <$> fetch <*> fetch

readMemAddr :: forall m. (MonadMachine m) => Addr -> m Addr
readMemAddr addr = toAddr <$> readMem addr <*> readMem (addr + fromIntegral 1)

getReg :: forall m a. (MonadEffect m) => (CPU -> Ref a) -> ReaderT CPU m a
getReg reg = do
    ref <- asks reg
    liftEffect $ Ref.read ref

setReg :: forall m a. (MonadEffect m) => (CPU -> Ref a) -> a -> ReaderT CPU m Unit
setReg reg v = do
    ref <- asks reg
    liftEffect $ Ref.write v ref

modifyReg :: forall m a. (MonadEffect m) => (CPU -> Ref a) -> (a -> a) -> ReaderT CPU m Unit
modifyReg reg f = setReg reg <<< f =<< getReg reg

push :: forall m. (MonadMachine m) => Word8 -> ReaderT CPU m Unit
push v = do
    ptr <- getReg _.sp
    writeMem (fromIntegral 0x100 + fromIntegral ptr) v
    setReg _.sp $ ptr - fromIntegral 1

pushAddr :: forall m. (MonadMachine m) => Addr -> ReaderT CPU m Unit
pushAddr addr = push hi *> push lo
  where
    hi = fromIntegral $ addr `shr` fromInt 8
    lo = fromIntegral $ addr .&. fromIntegral 0xff

pop :: forall m. (MonadMachine m) => ReaderT CPU m Word8
pop = do
    ptr <- getReg _.sp
    v <- readMem (fromIntegral 0x100 + fromIntegral (ptr + fromIntegral 1))
    setReg _.sp $ ptr + fromIntegral 1
    pure v

popAddr :: forall m. (MonadMachine m) => ReaderT CPU m Addr
popAddr = toAddr <$> pop <*> pop

-- getFlag :: (MonadEffect m) => (Lens' Word8 Bool) -> ReaderT CPU m Bool
-- getFlag flag = do
--     ref <- asks status
--     flags <- liftIO $ readIORef ref
--     return $ flags ^. flag

-- setFlag :: (MonadEffect m) => (Lens' Word8 Bool) -> Bool -> ReaderT CPU m Unit
-- setFlag flag b = do
--     ref <- asks status
--     liftIO $ modifyIORef ref $ set flag b

-- carry, zero, interruptEnable, decimal, overflow, negative :: Lens' Word8 Bool
-- carry = statusFlag 0
-- zero = statusFlag 1
-- interruptEnable = statusFlag 2
-- decimal = statusFlag 3
-- overflow = statusFlag 6
-- negative = statusFlag 7

-- statusFlag :: Int -> Lens' Word8 Bool
-- statusFlag i = lens (`testBit` i) (\x b -> if b then x `setBit` i else x `clearBit` i)
