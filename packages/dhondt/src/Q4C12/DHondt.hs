module Q4C12.DHondt
  ( Votes ( Votes )
  , Weighted
  , votesToWeighted
  , weightedToVotes
  , scale
  , dHondt
  )
  where

import Control.Monad.Trans.Writer.CPS
  ( tell
  )
import qualified Data.List as List
import qualified Data.Map as Map
import Data.PQueue.Prio.Max
  ( MaxPQueue
  )
import qualified Data.PQueue.Prio.Max as PQ
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder.Int as LTBI
import qualified Data.Text.Lazy.Builder.RealFloat as LTBF
import qualified Data.Text.Lazy.IO as LTIO
import GHC.Exts
  ( IsString ( fromString )
  )
import GHC.Real
  ( fromIntegral
  )
import Streaming
  ( Stream
  , Of ((:>))
  )
import qualified Streaming
import qualified Streaming.Prelude

-- Invariant: if weight is 0, values is empty.
data Weighted k = Weighted
  { _weight :: Double
  , _values :: Map k Double
  }

instance ( Ord k ) => Semigroup ( Weighted k ) where
  Weighted wa as <> Weighted wb bs = Weighted
    ( wa + wb )
    ( Map.unionWith (+) ( (*) wa <$> as ) ( (*) wb <$> bs ) )

instance ( Ord k ) => Monoid ( Weighted k ) where
  mempty = Weighted 0 mempty

scale :: Double -> Weighted k -> Weighted k
scale 0 _ = Weighted 0 Map.empty
scale x ( Weighted w vs ) = Weighted ( x * w ) vs

weightedToVotes :: Weighted Party -> Votes
weightedToVotes ( Weighted w vs ) = Votes ( (/ w) <$> vs )

votesToWeighted :: Votes -> Weighted Party
votesToWeighted vs = Weighted 1 ( getVotes vs )

newtype Party = Party { getParty :: SText }
  deriving stock ( Eq, Ord, Show )

instance IsString Party where
  fromString = Party . fromString

partyBuilder :: Party -> TBuilder
partyBuilder = LTB.fromText . getParty

data Seat = Seat
  { seatParty :: Party
  , seatQuotient :: Double
  , seatCount :: Natural
  }
  deriving stock ( Show )

newtype Votes = Votes { getVotes :: Map Party Double }

voteCount :: ( Functor f ) => Party -> ( Double -> f Double ) -> Votes -> f Votes
voteCount party f ( Votes votes ) =
  Votes <$> Map.alterF ( fmap Just . f . fromMaybe 0 ) party votes

newtype Seats = Seats { getSeats :: Map Party Natural }

seats :: ( Functor f ) => Party -> ( Natural -> f Natural ) -> Seats -> f Seats
seats party f ( Seats s ) =
  Seats <$> Map.alterF ( fmap Just . f . fromMaybe 0 ) party s

pqf :: ( Functor f, Ord k ) => ( a -> f k ) -> MaxPQueue k a -> Maybe ( f ( MaxPQueue k a ) )
pqf f = PQ.maxView
    >>> fmap ( \ ( a, q ) -> f a <&> \ k' -> PQ.insert k' a q )

partyStream
  :: ( Monad f, Ord k )
  => ( a -> f ( Of r k ) )
  -> MaxPQueue k a
  -> Stream ( Of r ) f ()
partyStream rekey =
  Streaming.unfold
      $ pqf ( Compose . rekey )
    >>> maybe ( Left () ) Right
    >>> fmap getCompose
    >>> sequenceA

naturalToDouble :: Natural -> Double
naturalToDouble = fromIntegral

-- TODO: unsafe
naturalToInt :: Natural -> Int
naturalToInt = fromIntegral

step
  :: ( Monad f )
  => Votes
  -> Party
  -> StateT Seats f ( Of Seat Double )
step votes party = do
  count <- seats party <+= 1
  let
    vs = view ( voteCount party ) votes
    prevQuotient =
      vs / naturalToDouble count
    newQuotient =
      vs / naturalToDouble ( count + 1 )
  pure ( Seat party prevQuotient count :> newQuotient )

seatStream :: ( Monad f ) => Votes -> Stream ( Of Seat ) ( StateT Seats f ) ()
seatStream votes =
  partyStream ( step votes ) initialQuotients
  where
  initialQuotients =
    Map.foldMapWithKey ( flip PQ.singleton ) ( getVotes votes )

dHondt :: Natural -> Votes -> IO ()
dHondt numSeats votes = do
  let
    stream :: ( Monad f ) => Stream ( Of Seat ) ( StateT Seats ( WriterT ( Maybe ( Last Seat ) ) f ) ) ()
    stream =
      Streaming.takes ( naturalToInt numSeats ) $ seatStream votes
  ( finalSeats, lastMay ) <- runWriterT $ flip execStateT ( Seats mempty ) $ flip Streaming.Prelude.mapM_ stream $ \ seat -> do
    liftIO $ LTIO.putStrLn $ LTB.toLazyText $ fold $
      [ "* Seat: ", partyBuilder ( seatParty seat ), " seat ", LTBI.decimal ( seatCount seat ), " on quotient ", LTBF.formatRealFloat LTBF.Fixed ( Just 2 ) ( seatQuotient seat )
      ]
    lift $ tell $ Just $ Last seat

  LTIO.putStrLn "Final allocation:"
  let
    allocs = List.sortOn ( Down . snd ) ( Map.assocs ( getSeats finalSeats ) )
  for_ allocs $ \ ( party, count ) ->
    LTIO.putStrLn $ LTB.toLazyText $ fold
      [ "* ", partyBuilder party, " with ", LTBI.decimal count
      ]

  for_ ( getLast <$> lastMay ) $ \ ( Seat finalParty quotient _ ) -> do

    let
      toGain party =
        let
          divisor = views ( seats party ) ( naturalToDouble . (+ 1) ) finalSeats
        in
          quotient * divisor - view ( voteCount party ) votes
      nextParties = List.sortOn
        toGain
        ( Map.keys $ Map.delete finalParty $ getVotes votes )

    LTIO.putStrLn "Next parties, and swing away from the final seat:"
    flip traverse_ nextParties $ \ party ->
      LTIO.putStrLn $ LTB.toLazyText $ fold
        [ "* ", partyBuilder party, " at ", LTBF.formatRealFloat LTBF.Fixed ( Just 2 ) ( toGain party )
        ]
