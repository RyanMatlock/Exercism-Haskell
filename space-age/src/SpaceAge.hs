module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune
  deriving (Eq)

ageOn :: Planet -> Float -> Float
ageOn planet seconds
  | planet == Mercury = 0.2408467 * secondsToEarthYears seconds
  | planet == Venus   = 0.61519726 * secondsToEarthYears seconds
  | planet == Earth   = secondsToEarthYears seconds
  | planet == Mars    = 1.8808158 * secondsToEarthYears seconds
  | planet == Jupiter = 11.862615 * secondsToEarthYears seconds
  | planet == Saturn  = 29.447498 * secondsToEarthYears seconds
  | planet == Uranus  = 84.016846 * secondsToEarthYears seconds
  | planet == Neptune = 164.79132 * secondsToEarthYears seconds
  | otherwise         = error "not implemented"
  where
    secondsToEarthYears sec = sec / 31557600

