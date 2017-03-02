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
  { asteroids   :: [Asteroid]   -- ^ Астероиды.
  , satellites  :: [Satellite]  -- ^ Спутники.
  , ufos        :: [UFO]        -- ^ НЛО.
  }

-- | Астероид.
data Asteroid = Asteroid
  { asteroidPosition :: Point   -- ^ Положение астероида.
  , asteroidVelocity :: Vector  -- ^ Вектор скорости.
  , asteroidSize     :: Float   -- ^ Размеры (диаметр).
  }

-- | Спутник.
data Satellite = Satellite
  { satellitePosition :: Point  -- ^ Положение спутника.
  , satelliteVelocity :: Vector -- ^ Вектор скорости спутника.
  , satelliteAngle    :: Float  -- ^ Угол поворота спутника.
  }

-- | НЛО.
data UFO = UFO
  { ufoPosition :: Point  -- ^ Положение НЛО.
  , ufoVelocity :: Vector -- ^ Вектор скорости.
  , ufoTarget   :: Point  -- ^ Цель НЛО.
  }

-- | Сгенерировать космический мусор.
initSpaceJunk :: SpaceJunk
initSpaceJunk = SpaceJunk
  { asteroids  = [ Asteroid (0, 0) (10, 2) 30 ]
  , satellites = [ Satellite (100, 100) (-5, -10) 30 ]
  , ufos       = [ UFO (-100, 100) (-5, -10) (0, 0) ]
  }

-- | Отобразить космический мусор.
drawSpaceJunk :: SpaceJunk -> Picture
drawSpaceJunk junk = pictures
  [ pictures (map drawAsteroid  (asteroids  junk))
  , pictures (map drawSatellite (satellites junk))
  , pictures (map drawUFO       (ufos       junk))
  ]

-- | Отобразить астероид.
drawAsteroid :: Asteroid -> Picture
drawAsteroid asteroid = color white (translate x y (thickCircle (r/2) r))
  where
    (x, y) = asteroidPosition asteroid
    r = asteroidSize asteroid / 2

-- | Отобразить спутник.
drawSatellite :: Satellite -> Picture
drawSatellite satellite = color (greyN 0.5) (translate x y (thickArc 0 theta (r/2) r))
  where
    (x, y) = satellitePosition satellite
    theta  = satelliteAngle satellite
    r = 30

-- | Отобразить НЛО.
drawUFO :: UFO -> Picture
drawUFO ufo = color green (translate x y (thickCircle (r/2) r))
  where
    (x, y) = ufoPosition ufo
    r = 20

-- | Обновить космический мусор.
updateSpaceJunk :: ViewPort -> Float -> SpaceJunk -> SpaceJunk
updateSpaceJunk _ dt junk = junk
  { asteroids  = map (updateAsteroid  dt) (asteroids  junk)
  , satellites = map (updateSatellite dt) (satellites junk)
  , ufos       = map (updateUFO       dt) (ufos       junk)
  }

-- | Вернуть объект на экран, если он вышел за границы.
-- Наш космос имеет топологию тора.
normalisePosition :: Point -> Point
normalisePosition (x, y) = (norm x screenWidth, norm y screenHeight)
  where
    norm z d = z - fromIntegral (floor (z / d + 0.5))

-- | Обновить положение астероида.
updateAsteroid :: Float -> Asteroid -> Asteroid
updateAsteroid dt asteroid = asteroid
  { asteroidPosition = normalisePosition (asteroidPosition asteroid + mulSV dt (asteroidVelocity asteroid)) }

-- | Обновить положение спутника.
updateSatellite :: Float -> Satellite -> Satellite
updateSatellite dt satellite = satellite
  { satellitePosition = normalisePosition (satellitePosition satellite + mulSV dt (satelliteVelocity satellite)) }

-- | Обновить положение НЛО.
updateUFO :: Float -> UFO -> UFO
updateUFO dt ufo = ufo
  { ufoPosition = normalisePosition (ufoPosition ufo + mulSV dt (ufoVelocity ufo))
  , ufoVelocity = ufoVelocity ufo + mulSV (dt * ufoAccel) (normalizeV (ufoTarget ufo - ufoPosition ufo))
  }

-- | Ширина экрана.
screenWidth :: Num a => a
screenWidth = 800

-- | Высота экрана.
screenHeight :: Num a => a
screenHeight = 450

-- | Ускорение НЛО.
ufoAccel :: Float
ufoAccel = 10
