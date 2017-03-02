module SpaceJunk where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Juicy

-- | Запустить моделирование с заданным начальным состоянием вселенной.
demo :: Images -> IO ()
demo images
  = simulate display bgColor fps initSpaceJunk (drawSpaceJunk images) updateSpaceJunk
  where
    display = InWindow "Космический мусор" (800, 450) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- | Загрузить изображения из файлов.
loadImages :: IO Images
loadImages = do
  Just asteroid   <- loadJuicyPNG "images/asteroid.png"
  Just satellite  <- loadJuicyPNG "images/satellite.png"
  Just ufo        <- loadJuicyPNG "images/ufo.png"
  return Images
    { imageAsteroid   = scale 0.1 0.1 asteroid
    , imageSatellite  = scale 0.3 0.3 satellite
    , imageUFO        = scale 0.03 0.03 ufo
    }

-- | Изображения объектов.
data Images = Images
  { imageAsteroid  :: Picture   -- ^ Изображение астероида.
  , imageSatellite :: Picture   -- ^ Изображение спутника.
  , imageUFO       :: Picture   -- ^ Изображение НЛО.
  }

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
  { asteroids  = [ Asteroid (0, 0) (10, 2) 3 ]
  , satellites = [ Satellite (100, 100) (-5, -10) 30 ]
  , ufos       = [ UFO (-100, 100) (-5, -10) (0, 0) ]
  }

-- | Отобразить космический мусор.
drawSpaceJunk :: Images -> SpaceJunk -> Picture
drawSpaceJunk images junk = pictures
  [ pictures (map (drawAsteroid  (imageAsteroid  images)) (asteroids  junk))
  , pictures (map (drawSatellite (imageSatellite images)) (satellites junk))
  , pictures (map (drawUFO       (imageUFO       images)) (ufos       junk))
  ]

-- | Отобразить астероид.
drawAsteroid :: Picture -> Asteroid -> Picture
drawAsteroid image asteroid = translate x y (scale r r image)
  where
    (x, y) = asteroidPosition asteroid
    r = asteroidSize asteroid

-- | Отобразить спутник.
drawSatellite :: Picture -> Satellite -> Picture
drawSatellite image satellite = translate x y (rotate theta image)
  where
    (x, y) = satellitePosition satellite
    theta  = satelliteAngle satellite

-- | Отобразить НЛО.
drawUFO :: Picture -> UFO -> Picture
drawUFO image ufo = translate x y image
  where
    (x, y) = ufoPosition ufo

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
  { satellitePosition = normalisePosition (satellitePosition satellite + mulSV dt (satelliteVelocity satellite))
  , satelliteAngle    = satelliteAngle satellite + satelliteRotationSpeed}

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

-- | Скорость вращения спутников.
satelliteRotationSpeed :: Float
satelliteRotationSpeed = 0.5

