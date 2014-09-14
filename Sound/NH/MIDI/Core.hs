{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Sound.NH.MIDI.Core where

import Data.Bits    ((.|.), shiftL)
import Data.Data    (Data, Typeable)
import GHC.Generics (Generic)
import Data.Word    (Word8, Word16)

-- | MIDI Channel. The value will be 0-15 even though MIDI channels are 1-16.
newtype Channel = Channel { _channel :: Word8 }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | Note Number
newtype NoteNumber = NoteNumber { _noteNumber :: Word8 }
   deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | Velocity
newtype Velocity   = Velocity { _velocity :: Word8 }
   deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | Velocity
newtype Pressure   = Pressure { _pressure :: Word8 }
   deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | continuous controller number
newtype Controller = Controller { _controller :: Word8 }
   deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | a patch number
newtype PatchNum   = PatchNum { _patchNum :: Word8 }
   deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | an 8-bit value
newtype Value      = Value { _value :: Word8 }
   deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | a 14-bit value
newtype Value14    = Value14 { _value14 :: Word16 }
   deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

mkValue14 :: Word8 -> Word8 -> Value14
mkValue14 lo hi  = Value14 (((fromIntegral hi) `shiftL` 7) .|. (fromIntegral lo))

-- | MIDI messages
data MIDI
    = NoteOff Channel NoteNumber Velocity
    | NoteOn  Channel NoteNumber Velocity
    | AfterTouch Channel NoteNumber Pressure
    | ControlChange Channel Controller Value
    | ProgramChange Channel PatchNum
    | ChannelPressure Channel Pressure
    | PitchWheel Channel Value14
    | MTCQuarterFrameMessage Value
    | SongPositionPointer Value14
    | SongSelect Value
    | TuneRequest
    | MidiClock -- 24 clocks == 1 quarter note
    | MidiTick  -- 1 tick    == 10 milliseconds
    | MidiStart
    | MidiStop
    | MidiContinue
    | ActiveSense
    | MidiReset
    | Unknown Word8
      deriving Show
