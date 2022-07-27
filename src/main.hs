{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Codec.Midi as Midi
import Control.Monad (forM_)
import Data.Interval (Extended (Finite), Interval, (<=..<), (<=..<=))
import qualified Data.Map as Map
import qualified Data.Map.Delta as Delta
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.SegmentTree (queryTree)
import qualified Data.SegmentTree as SegmentTree
import Data.Tuple.Extra (second)
import qualified Debug.Trace as Debug
import System.Random (mkStdGen, uniformR)

rationalRemainder :: Rational -> Rational -> Rational
rationalRemainder x y =
  let divided = x / y
   in x - ((floor divided % 1) * y)

type Channel = Integer

type Note = Integer

type Velocity = Integer

-- Do we need this?
data MidiNote = MidiNote
  { channel :: Channel,
    note :: Note,
    velocity :: Velocity
  }
  deriving (Eq, Ord, Show)

type MidiSound = Map.Map (Channel, Note) Velocity

data NoteEvent
  = NoteOn
  | NoteOff
  deriving (Show)

type Composition a = (Rational -> a)

sample :: Rational -> Rational -> Composition a -> [(Rational, a)]
sample rate end composition = [(t, composition t) | t <- [0, rate .. (end - rate)]]

samplesToEvents ::
  [(Rational, MidiSound)] ->
  [(Rational, (Channel, Note, Velocity, NoteEvent))]
samplesToEvents samples = fst $ foldl go ([], Map.empty :: MidiSound) samples
  where
    go (events, prevMidiSound) (when, midiSound) = (events ++ newEvents when prevMidiSound midiSound, midiSound)

    newEvents when prevMidiSound midiSound =
      let diff = Delta.diff prevMidiSound midiSound
          old = Delta.toOld diff
          new = Delta.toNew diff
          different = Delta.toDelta diff
       in [(when, (channel, note, velocity, NoteOff)) | ((channel, note), velocity) <- Map.toList old]
            ++ [(when, (channel, note, velocity, NoteOn)) | ((channel, note), velocity) <- Map.toList new]
            ++ [(when, (channel, note, newVel, NoteOn)) | ((channel, note), Delta.DeltaUnit _oldVel newVel) <- Map.toList different]

type NoteStartMap = Map.Map (Channel, Note) (Rational, Velocity)

trackAbsoluteTime :: Midi.Track Midi.Ticks -> Midi.Track Midi.Ticks
trackAbsoluteTime track = reverse $ snd $ foldl trackAbsoluteTimeFolder (0, []) track
  where
    trackAbsoluteTimeFolder (absoluteTime, events) (diff, event) =
      (absoluteTime + diff, (absoluteTime + diff, event) : events)

midiToMidiSoundComposition ::
  Midi.Track Midi.Ticks ->
  Composition MidiSound
midiToMidiSoundComposition track =
  let noteDurations :: [((Rational, Rational), MidiNote)]
      noteDurations = snd $ foldl noteDurationsFolder (Map.empty :: NoteStartMap, []) track
      noteDurationsFolder ::
        (NoteStartMap, [((Rational, Rational), MidiNote)]) ->
        (Midi.Ticks, Midi.Message) ->
        (NoteStartMap, [((Rational, Rational), MidiNote)])

      noteDurationsFolder (noteState, notes) (when, Midi.NoteOff _channel _key _velocity) =
        let (startTime, velocity) = fromMaybe (0, 0) $ Map.lookup (fromIntegral _channel, fromIntegral _key) noteState
         in (Map.delete (fromIntegral _channel, fromIntegral _key) noteState, ((startTime, fromIntegral when), MidiNote {channel = fromIntegral _channel, note = fromIntegral _key, velocity = velocity}) : notes)
      noteDurationsFolder (noteState, notes) (when, Midi.NoteOn _channel _key 0) =
        let (startTime, velocity) = fromMaybe (0, 0) $ Map.lookup (fromIntegral _channel, fromIntegral _key) noteState
         in (Map.delete (fromIntegral _channel, fromIntegral _key) noteState, ((startTime, fromIntegral when), MidiNote {channel = fromIntegral _channel, note = fromIntegral _key, velocity = fromIntegral velocity}) : notes)
      noteDurationsFolder (noteState, notes) (when, Midi.NoteOn _channel _key _velocity) =
        (Map.insert (fromIntegral _channel, fromIntegral _key) (fromIntegral when, fromIntegral _velocity) noteState, notes)
      noteDurationsFolder (noteState, notes) (when, _event) = (noteState, notes)

      treeMapper :: ((Rational, Rational), MidiNote) -> (Interval Rational, MidiSound)
      treeMapper ((start, end), midiNote) = (Finite start <=..<= Finite end, Map.singleton (channel midiNote, note midiNote) (velocity midiNote))
      tree :: SegmentTree.STree MidiSound Rational
      tree = SegmentTree.fromList (treeMapper <$> noteDurations)
   in fromMaybe Map.empty <$> queryTree tree

sustain :: MidiNote -> Rational -> Composition MidiSound
sustain midiNote dur t =
  if t < dur
    then Map.singleton (channel midiNote, note midiNote) (velocity midiNote)
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
  eMidi <- Midi.importFile "gnossienne-1.mid"
  case eMidi of
    Left s -> putStrLn $ "Error: " ++ s
    Right mi -> do
      putStrLn ""
      let track = Midi.tracks mi !! 1
          trackAbsolute = trackAbsoluteTime track
          composition = midiToMidiSoundComposition trackAbsolute
          composition2 = \t ->
            let measureLength = 480 * 4
                measure = floor (t / measureLength) % 1
                tick = t - (measure * measureLength)
                newTime = ((measure + 1) * measureLength) - tick
             in composition newTime
          endTime = fromIntegral $ maximum $ map fst trackAbsolute
          reversed t = composition (endTime - t)
          -- Create a composition where each measure alternates between
          -- being from the beginning and from the end
          inward t =
            let measureLength = 320
                measure = floor (t / measureLength) % 1
             in if even (floor measure) then composition t else reversed t
          -- Now plus a quieter version of 1/3 of a measure ago
          echos t = composition t `mappend` ((`div` 3) <$> composition (t - 960))
          flange t =
            let delay = sin (fromRational (t / (240)) :: Double) * 120
             in composition (t + toRational delay)
          slowlyDecaysIntoChaos t =
            let pureGen = mkStdGen (floor t)
                (randomVar :: Integer, _) = uniformR (0, floor $ endTime - t) pureGen
             in if (randomVar == 0) then (reversed t) else (composition t)
      let notes = sample 1 endTime flange

      putStrLn "Ticks per beat: "
      print (Midi.timeDiv mi)

      -- putStrLn "Track:"
      -- forM_ track $ \(when, message) -> do
      --   putStrLn $ show when ++ " " ++ show message
      -- putStrLn ""
      -- putStrLn ""

      -- putStrLn "Absolute time track:"
      -- forM_ trackAbsolute $ \(when, message) -> do
      --   putStrLn $ show when ++ " " ++ show message
      -- putStrLn ""

      putStrLn "What's happening at 1900"
      print (composition 1900)

      let events = samplesToEvents notes

      -- putStrLn "Events"
      -- forM_ (take 40 events) $ \(t, (channel, note, velocity, event)) -> do
      --   putStrLn $ prettyPrintRational t ++ ": " ++ show channel ++ " " ++ show note ++ " " ++ show velocity ++ " " ++ show event
      -- putStrLn ""

      let eventToMidiMessage :: (Channel, Note, Velocity, NoteEvent) -> Midi.Message
          eventToMidiMessage (channel, note, velocity, NoteOn) = Midi.NoteOn (fromIntegral channel) (fromIntegral note) (fromIntegral velocity)
          eventToMidiMessage (channel, note, velocity, NoteOff) = Midi.NoteOff (fromIntegral channel) (fromIntegral note) (fromIntegral velocity)

      let midiEvents = map (second eventToMidiMessage) events

          midiEventsWithOffsets :: [(Rational, Midi.Message)] -> [(Int, Midi.Message)]
          midiEventsWithOffsets [] = []
          midiEventsWithOffsets xs = reverse $ snd $ foldl offset (0, []) xs

          offset :: (Int, [(Int, Midi.Message)]) -> (Rational, Midi.Message) -> (Int, [(Int, Midi.Message)])
          offset (oldTime, message) (now, newMessage) =
            (floor now, (floor now - oldTime, newMessage) : message)

          track = midiEventsWithOffsets midiEvents

          midiFile =
            Midi.Midi
              { Midi.fileType = Midi.MultiTrack,
                Midi.timeDiv = Midi.TicksPerBeat 480,
                Midi.tracks = [(0, Midi.ProgramChange 0 1) : track]
              }

      -- putStrLn "track:"
      -- forM_ track $ \(t, m) -> do
      --   putStrLn $ show t ++ ": " ++ show m
      Midi.exportFile "test.mid" midiFile
