module SpaceJunk where

import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Data.Vector

-- | Запустить моделирование с заданным начальным состоянием вселенной.
demo :: IO ()
demo = simulate display bgColor fps initSpaceJunk drawSpaceJunk updateSpaceJunk
  where
    display = InWindow "Космический мусор" (800, 450) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- | Космический мусор.
data SpaceJunk = SpaceJunk
  { asteroids :: [Asteroid]
  }

-- | Астероид.
data Asteroid = Asteroid
  { asteroidPosition :: Point   -- ^ Положение астероида.
  , asteroidVelocity :: Vector  -- ^ Вектор скорости.
  , asteroidSize     :: Float   -- ^ Размеры (диаметр).
  }

-- | Сгенерировать космический мусор.
initSpaceJunk :: SpaceJunk
initSpaceJunk = SpaceJunk
  { asteroids = [ Asteroid (0, 0) (10, 2) 30 ]
  }

-- | Отобразить космический мусор.
drawSpaceJunk :: SpaceJunk -> Picture
drawSpaceJunk junk = pictures
  [ pictures (map drawAsteroid (asteroids junk)) ]

-- | Отобразить астероид.
drawAsteroid :: Asteroid -> Picture
drawAsteroid asteroid = color white (translate x y (thickCircle (r/2) r))
  where
    (x, y) = asteroidPosition asteroid
    r = asteroidSize asteroid / 2

-- | Обновить космический мусор.
updateSpaceJunk :: ViewPort -> Float -> SpaceJunk -> SpaceJunk
updateSpaceJunk _ dt junk = junk
  { asteroids = map (updateAsteroid dt) (asteroids junk) }

-- | Обновить положение астероида.
updateAsteroid :: Float -> Asteroid -> Asteroid
updateAsteroid dt asteroid = asteroid
  { asteroidPosition = asteroidPosition asteroid + mulSV dt (asteroidVelocity asteroid) }
