data Direction = North | South | East | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise South = West
turnClockwise East = South
turnClockwise West = North
