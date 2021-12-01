{-# LINE 1 "Basement/Terminal/Size.hsc" #-}
{-# LANGUAGE CApiFFI #-}
module Basement.Terminal.Size 
    ( getDimensions
    ) where
        
import           Foreign
import           Foreign.C
import           Basement.Compat.Base
import           Basement.Types.OffsetSize
import           Basement.Numerical.Subtractive
import           Basement.Numerical.Additive
import           Prelude (fromIntegral)



{-# LINE 22 "Basement/Terminal/Size.hsc" #-}


{-# LINE 26 "Basement/Terminal/Size.hsc" #-}

{-# LINE 27 "Basement/Terminal/Size.hsc" #-}




{-# LINE 33 "Basement/Terminal/Size.hsc" #-}


{-# LINE 35 "Basement/Terminal/Size.hsc" #-}
data Winsize = Winsize
    { ws_row    :: !Word16
    , ws_col    :: !Word16
    , ws_xpixel :: !Word16
    , ws_ypixel :: !Word16
    }

instance Storable Winsize where
    sizeOf _ = (8)
{-# LINE 44 "Basement/Terminal/Size.hsc" #-}
    alignment _ = 2
{-# LINE 45 "Basement/Terminal/Size.hsc" #-}
    peek ptr = do
        r <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 47 "Basement/Terminal/Size.hsc" #-}
        c <- (\hsc_ptr -> peekByteOff hsc_ptr 2) ptr
{-# LINE 48 "Basement/Terminal/Size.hsc" #-}
        x <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 49 "Basement/Terminal/Size.hsc" #-}
        y <- (\hsc_ptr -> peekByteOff hsc_ptr 6) ptr
{-# LINE 50 "Basement/Terminal/Size.hsc" #-}
        return (Winsize r c x y)
    poke ptr (Winsize r c x y) = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r
{-# LINE 53 "Basement/Terminal/Size.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 2) ptr c
{-# LINE 54 "Basement/Terminal/Size.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr x
{-# LINE 55 "Basement/Terminal/Size.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 6) ptr y
{-# LINE 56 "Basement/Terminal/Size.hsc" #-}
        

{-# LINE 129 "Basement/Terminal/Size.hsc" #-}
-- defined FOUNDATION_SYSTEM_WINDOWS


{-# LINE 132 "Basement/Terminal/Size.hsc" #-}

foreign import capi "sys/ioctl.h ioctl" c_ioctl :: CInt -> CULong -> Ptr a -> IO CInt

-- | Get the terminal windows size
tiocgwinsz :: CULong
tiocgwinsz = Prelude.fromIntegral (21523 :: Word)
{-# LINE 138 "Basement/Terminal/Size.hsc" #-}


{-# LINE 143 "Basement/Terminal/Size.hsc" #-}


{-# LINE 145 "Basement/Terminal/Size.hsc" #-}
ioctlWinsize :: CInt -> IO (Maybe (CountOf Char, CountOf Char))
ioctlWinsize fd = alloca $ \winsizePtr -> do
    status <- c_ioctl fd tiocgwinsz winsizePtr
    if status == (-1 :: CInt)
        then pure Nothing
        else Just . toDimensions <$> peek winsizePtr
  where
    toDimensions winsize =
        ( CountOf . Prelude.fromIntegral . ws_col $ winsize
        , CountOf . Prelude.fromIntegral . ws_row $ winsize)
       

{-# LINE 174 "Basement/Terminal/Size.hsc" #-}
-- defined FOUNDATION_SYSTEM_WINDOWS

-- | Return the size of the current terminal
--
-- If the system is not supported or that querying the system result in an error
-- then a default size of (80, 24) will be given back.
getDimensions :: IO (CountOf Char, CountOf Char)
getDimensions =

{-# LINE 185 "Basement/Terminal/Size.hsc" #-}
    maybe defaultSize id <$> ioctlWinsize 0

{-# LINE 189 "Basement/Terminal/Size.hsc" #-}
  where
    defaultSize = (80, 24)
