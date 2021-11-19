module Main (main) where

import qualified Codec.Midi as Midi
import Control.Monad (forM_)
import qualified Data.Map as Map
import Data.Ratio ((%))
import Data.Tuple.Extra (second)

rationalRemainder :: Rational -> Rational -> Rational
rationalRemainder x y =
  let divided = x / y
   in x - ((floor divided % 1) * y)

type Channel = Integer

type Note = Integer

type Velocity = Integer

-- Do we need this?
data MidiNote = MidiNote
  { note :: Note,
    velocity :: Velocity,
    channel :: Channel
  }
  deriving (Eq, Ord, Show)

type MidiSound = Map.Map Channel (Map.Map Note Velocity)

-- newtype Audio = Audio (Maybe Sound) deriving (Show, Semigroup)

-- instance Monoid Audio where
--   mempty = Audio Nothing

data NoteEvent
  = NoteOn
  | NoteOff
  deriving (Show)

type Composition a = (Rational -> a)

sample :: Rational -> Rational -> Composition a -> [(Rational, a)]
sample rate end composition = [(t, composition t) | t <- [0, rate .. (end - rate)]]

samplesToEvents ::
  [(Rational, MidiSound)] ->
  [(Rational, (Note, NoteEvent))]
samplesToEvents samples = reverse $ fst $ foldl go ([], Map.empty :: MidiSound) samples
  where
    go (events, prevMidiSound) (when, midiSound) = (newEvents when prevMidiSound midiSound : events, midiSound)
    newEvents _when _prevMidiSound _midiSound = undefined

sustain :: MidiNote -> Rational -> Composition MidiSound
sustain midiNote dur t =
  if t < dur
    then Map.singleton (channel midiNote) (Map.singleton (note midiNote) (velocity midiNote))
    else Map.empty

loopAt :: Rational -> Composition a -> Composition a
loopAt duration composition t = composition (rationalRemainder t duration)

_compositionA :: Composition MidiSound
_compositionA =
  let note = sustain (MidiNote 0 0 0) (1 % 2)
   in loopAt (1 % 4) note

prettyPrintRational :: Rational -> String
prettyPrintRational r =
  let wholeNumber = floor r
      fraction = r - (wholeNumber % 1)
   in show wholeNumber ++ "." ++ show (floor (fraction * 100))

main :: IO ()
main = do
  putStrLn "Samples:"
  let notes = sample (1 % 64) 4 (loopAt (1 % 1) (sustain (MidiNote 0 0 0) (1 % 2)))
  forM_ notes $ \(t, audio) -> do
    putStrLn $ prettyPrintRational t ++ ": " ++ show audio

  putStrLn ""
  putStrLn "Events:"

  let events = samplesToEvents notes
  forM_ events $ \(t, (note, event)) -> do
    putStrLn $ prettyPrintRational t ++ ": " ++ show note ++ " " ++ show event

  let eventToMidiMessage :: (Note, NoteEvent) -> Midi.Message
      eventToMidiMessage (note, NoteOn) = Midi.NoteOn 9 (fromIntegral note) 100
      eventToMidiMessage (note, NoteOff) = Midi.NoteOff 9 (fromIntegral note) 0

  let midiEvents = map (second eventToMidiMessage) events

      midiEventsWithOffsets :: [(Rational, Midi.Message)] -> [(Int, Midi.Message)]
      midiEventsWithOffsets [] = []
      midiEventsWithOffsets xs = reverse $ snd $ foldl offset (0, []) xs

      offset :: (Int, [(Int, Midi.Message)]) -> (Rational, Midi.Message) -> (Int, [(Int, Midi.Message)])
      offset (oldTime, message) (now, newMessage) =
        let newTime = floor $ now * 24
         in (newTime, (newTime - oldTime, newMessage) : message)

      track = midiEventsWithOffsets midiEvents

  let _trackOne =
        [ (0, Midi.NoteOn {Midi.channel = 0, Midi.key = 69, Midi.velocity = 100}),
          (24, Midi.NoteOn {Midi.channel = 0, Midi.key = 71, Midi.velocity = 100}),
          (24, Midi.NoteOn {Midi.channel = 0, Midi.key = 73, Midi.velocity = 100}),
          (24, Midi.NoteOn {Midi.channel = 0, Midi.key = 74, Midi.velocity = 100})
        ]
      midiFile =
        Midi.Midi
          { Midi.fileType = Midi.MultiTrack,
            Midi.timeDiv = Midi.TicksPerBeat 24,
            Midi.tracks = [(0, Midi.ProgramChange 9 0) : track]
          }

  putStrLn "track:"
  forM_ track $ \(t, m) -> do
    putStrLn $ show t ++ ": " ++ show m
  Midi.exportFile "test.mid" midiFile
