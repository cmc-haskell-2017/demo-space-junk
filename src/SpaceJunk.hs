module SpaceJunk where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Juicy

-- | Запустить моделирование с заданным начальным состоянием вселенной.
demo :: Images -> IO ()
demo images
  = simulate display bgColor fps initSpaceJunk (drawSpaceJunk images) updateSpaceJunk
  where
    display = InWindow "Космический мусор" (screenWidth, screenHeight) (100, 100)
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
    , imageSatellite  = scale 0.1 0.1 satellite
    , imageUFO        = scale 0.02 0.02 ufo
    }

-- =========================================
-- Модель
-- =========================================

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

-- | Физический объект с изменяемым положением и скоростью.
class Physical a where
  -- | Получить положение объекта.
  getPosition  :: a -> Point
  -- | Установить положение объекта.
  setPosition :: Point -> a -> a
  -- | Получить вектор скорости объекта.
  getVelocity  :: a -> Vector

-- | Переместить физический объект.
--
-- prop> \(object :: Asteroid) -> move 0 object == object
-- prop> \(object :: Asteroid) -> move x (move y object) ~= move (x + y) object
move :: Physical a => Float -> a -> a
move dt object = setPosition new object
  where
    new = getPosition object + mulSV dt (getVelocity object)

-- | Астероид.
data Asteroid = Asteroid
  { asteroidPosition :: Point   -- ^ Положение астероида.
  , asteroidVelocity :: Vector  -- ^ Вектор скорости.
  , asteroidSize     :: Float   -- ^ Размеры (диаметр).
  } deriving (Eq, Show)

instance Physical Asteroid where
  getPosition = asteroidPosition
  getVelocity = asteroidVelocity
  setPosition new asteroid = asteroid
    { asteroidPosition = new }

-- | Спутник.
data Satellite = Satellite
  { satellitePosition :: Point  -- ^ Положение спутника.
  , satelliteVelocity :: Vector -- ^ Вектор скорости спутника.
  , satelliteAngle    :: Float  -- ^ Угол поворота спутника.
  }

instance Physical Satellite where
  getPosition = satellitePosition
  getVelocity = satelliteVelocity
  setPosition new satellite = satellite
    { satellitePosition = new }

-- | НЛО.
data UFO = UFO
  { ufoPosition :: Point  -- ^ Положение НЛО.
  , ufoVelocity :: Vector -- ^ Вектор скорости.
  , ufoTarget   :: Point  -- ^ Цель НЛО.
  }

instance Physical UFO where
  getPosition = ufoPosition
  getVelocity = ufoVelocity
  setPosition new ufo = ufo
    { ufoPosition = new }

-- | Сгенерировать космический мусор.
initSpaceJunk :: SpaceJunk
initSpaceJunk = SpaceJunk
  { asteroids  =
      [ Asteroid ( 200,    0) (  6,   6) 3
      , Asteroid (-200,   25) (  9,   9) 2
      , Asteroid (   0,  -25) ( 12,  12) 1.5
      , Asteroid (   0,   50) ( 12,  10) 1.5
      , Asteroid (-100, -100) (  9,  10) 2.1
      , Asteroid (-400, -225) (4.5,   5) 4
      , Asteroid ( 100, -100) (  9,   9) 2
      , Asteroid (-300,  -75) (  6,   6) 3
      , Asteroid (-100, -125) ( 12,  12) 1.5
      , Asteroid (-100,  -50) ( 12,  10) 1.5
      , Asteroid (-200, -200) (  9,  10) 2.1
      ]
  , satellites =
      [ Satellite (-450, 250)  ( 10, -15) 100
      , Satellite ( 500, -300) (-10,  15) 0
      ]
  , ufos =
      [ UFO (500, 400) (5, -15) (0, 0) ]
  }

-- =========================================
-- Функции отрисовки
-- =========================================

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

-- =========================================
-- Функции обновления
-- =========================================

-- | Обновить космический мусор.
updateSpaceJunk :: ViewPort -> Float -> SpaceJunk -> SpaceJunk
updateSpaceJunk _ dt junk = junk
  { asteroids  = map (updateAsteroid  dt) (asteroids  junk)
  , satellites = map (updateSatellite dt) (satellites junk)
  , ufos       = map (updateUFO       dt) (ufos       junk)
  }

-- | Обновить положение астероида.
updateAsteroid :: Float -> Asteroid -> Asteroid
updateAsteroid = move

-- | Обновить положение спутника.
updateSatellite :: Float -> Satellite -> Satellite
updateSatellite dt satellite = move dt satellite
  { satelliteAngle = satelliteAngle satellite + satelliteRotationSpeed}

-- | Обновить положение НЛО.
updateUFO :: Float -> UFO -> UFO
updateUFO dt ufo = move dt ufo
  { ufoVelocity = ufoVelocity ufo + mulSV (dt * ufoAccel) (normalizeV (ufoTarget ufo - ufoPosition ufo)) }

-- =========================================
-- Параметры моделирования
-- =========================================

-- | Ширина экрана.
screenWidth :: Num a => a
screenWidth = 800

-- | Высота экрана.
screenHeight :: Num a => a
screenHeight = 450

-- | Ускорение НЛО.
ufoAccel :: Float
ufoAccel = 15

-- | Скорость вращения спутников.
satelliteRotationSpeed :: Float
satelliteRotationSpeed = 0.1

-- =========================================
-- Секция для настроек автоматических тестов
-- =========================================

-- $setup
-- >>> :set -XScopedTypeVariables
-- >>> import Test.QuickCheck
-- >>> class AlmostEq a where (~=) :: a -> a -> Bool
-- >>> instance AlmostEq Float where x ~= y = x == y || abs (x - y) / max (abs x) (abs y) < 0.001
-- >>> instance (AlmostEq a, AlmostEq b) => AlmostEq (a, b) where (x, y) ~= (u, v) = x ~= u && y ~= v
-- >>> instance AlmostEq Asteroid where Asteroid p1 v1 s1 ~= Asteroid p2 v2 s2 = p1 ~= p2 && v1 ~= v2 && s1 ~= s2
-- >>> instance Arbitrary Asteroid where arbitrary = Asteroid <$> arbitrary <*> arbitrary <*> arbitrary

