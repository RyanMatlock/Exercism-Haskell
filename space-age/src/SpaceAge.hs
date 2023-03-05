module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

-- using case of is the right way to do this because the compiler forces you to
-- check for all cases
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
  where
    secondsToEarthYears sec = sec / 31557600

