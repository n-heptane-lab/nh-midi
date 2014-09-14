{- | A pipes-based module for parsing a MIDI stream -}
module Sound.NH.MIDI.Parse
       ( parseMidi
       ) where

import Control.Applicative ((<$>))
import Control.Monad       (forever)
import Data.Bits           ((.&.), (.|.), shiftL)
import Data.Word           (Word8)
import Pipes               (Pipe, await, yield)
import Sound.NH.MIDI.Core  ( MIDI(..), Channel(..), Controller(..), NoteNumber(..)
                           , PatchNum(..), Pressure(..), Value(..), Value14(..)
                           , Velocity(..), mkValue14)

-- | convert a raw midi bytestring to a 'MIDI' stream
parseMidi :: (Monad m) => Pipe Word8 MIDI m ()
parseMidi = forever $
  do w <- await
     case w .&. 0xF0 of
       0x80 -> do noteNumber <- NoteNumber <$> await
                  velocity   <- Velocity   <$> await
                  yield (NoteOff (mkChannel w) noteNumber velocity)
       0x90 -> do noteNumber <- NoteNumber <$> await
                  velocity   <- Velocity   <$> await
                  yield (NoteOn (mkChannel w) noteNumber velocity)
       0xA0 -> do noteNumber <- NoteNumber <$> await
                  pressure   <- Pressure   <$> await
                  yield (AfterTouch (mkChannel w) noteNumber pressure)
       0xB0 -> do controller <- Controller <$> await
                  value      <- Value      <$> await
                  yield (ControlChange (mkChannel w) controller value)
       0xC0 -> do patchNum   <- PatchNum   <$> await
                  yield (ProgramChange (mkChannel w) patchNum)
       0xD0 -> do pressure   <- Pressure   <$> await
                  yield (ChannelPressure (mkChannel w) pressure)
       0xE0 -> do v          <- value14
                  yield (PitchWheel (mkChannel w) v)
       0xF0 ->
         case w of
           0xF1 -> do v     <- Value       <$> await
                      yield (MTCQuarterFrameMessage v)
           0xF2 -> do v     <- value14
                      yield (SongPositionPointer v)
           0xF3 -> do v     <- Value       <$> await
                      yield (SongSelect v)
           0xF6 -> yield TuneRequest
           0xF8 -> yield MidiClock
           0xF9 -> yield MidiTick
           0xFA -> yield MidiStart
           0xFB -> yield MidiContinue
           0xFC -> yield MidiStop
           0xFE -> yield ActiveSense
           0xFF -> yield MidiReset
       _ -> yield (Unknown w)
  where
    mkChannel w = Channel (w .&. 0x0F)

value14 :: (Monad m) => Pipe Word8 b m Value14
value14 =
  do lo <- await
     hi <- await
     return $ mkValue14 lo hi
