module Q4C12.Position
  ( Position (Position), posLine, posColumn
  , PositionRange (PositionRange), posRangeStart, posRangeEnd
  , position, positionRange
  , startPosition, updatePosition
  )
  where

import qualified Data.Text as ST
import Formatting (Format, later, bprint, int)
import GHC.Real (fromIntegral)

--0-based indices.
data Position = Position
  { posLine :: Word64
  , posColumn :: Word64
  }
  deriving (Show, Eq, Generic)

instance NFData Position

instance Ord Position where
  Position line col <= Position line' col' = line <= line' || (line == line' && col <= col')

startPosition :: Position
startPosition = Position 0 0

--Note: the end's position is *not* included in the thing. So the "bar" in "foobarbaz" would be PositionRange (0, 3) (0, 6).
data PositionRange = PositionRange
  { posRangeStart :: Position
  , posRangeEnd :: Position
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData PositionRange

updatePosition :: Position -> SText -> Position
updatePosition initial
  = flip appEndo initial
  . getDual
  . intercalateMap0 (Dual $ Endo nextLine) (Dual . Endo . addCols)
  . ST.split (== '\n')
  where
    --TODO: fromIntegral is scary. It's safe here because length >= 0, but... yeah.
    addCols t pos = Position (posLine pos) (posColumn pos + fromIntegral (ST.length t))
    nextLine pos = Position (posLine pos + 1) 0

position :: Format r (Position -> r)
position = later $ \(Position line col) ->
  bprint ("(" . int . ", " . int . ")") (line + 1) (col + 1)

positionRange :: Format r (PositionRange -> r)
positionRange = later $ \(PositionRange (Position line col) (Position line' col')) ->
  bprint ("(" . int . ":" . int . "-" . int . ":" . int . ")") (line + 1) (col + 1) (line' + 1) (col' + 1)
