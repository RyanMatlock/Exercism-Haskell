module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune
  -- deriving (Eq) -- necessary for guard version

-- Note to future self: div is integral division; / is float division
-- ageOn :: Planet -> Float -> Float
-- ageOn planet seconds
--   | planet == Mercury = secondsToEarthYears seconds / 0.2408467
--   | planet == Venus   = secondsToEarthYears seconds / 0.61519726
--   | planet == Earth   = secondsToEarthYears seconds
--   | planet == Mars    = secondsToEarthYears seconds / 1.8808158
--   | planet == Jupiter = secondsToEarthYears seconds / 11.862615
--   | planet == Saturn  = secondsToEarthYears seconds / 29.447498
--   | planet == Uranus  = secondsToEarthYears seconds / 84.016846
--   | planet == Neptune = secondsToEarthYears seconds / 164.79132
--   | otherwise         = error "not implemented"
--   where
--     secondsToEarthYears sec = sec / 31557600
-- works!

ageOn :: Planet -> Float -> Float
ageOn planet seconds = case planet of
  Mercury -> secondsToEarthYears seconds / 0.2408467
  Venus   -> secondsToEarthYears seconds / 0.61519726
  Earth   -> secondsToEarthYears seconds
  Mars    -> secondsToEarthYears seconds / 1.8808158
  Jupiter -> secondsToEarthYears seconds / 11.862615
  Saturn  -> secondsToEarthYears seconds / 29.447498
  Uranus  -> secondsToEarthYears seconds / 84.016846
  Neptune -> secondsToEarthYears seconds / 164.79132
  -- _       -> error "not implemented"
  where
    secondsToEarthYears sec = sec / 31557600
-- works and doesn't require modifying Planet
