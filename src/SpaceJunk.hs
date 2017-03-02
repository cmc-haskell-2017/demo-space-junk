module SpaceJunk where

import Graphics.Gloss.Interface.Pure.Simulate

-- | Запустить моделирование с заданным начальным состоянием вселенной.
demo :: IO ()
demo = simulate display bgColor fps initSpaceJunk drawSpaceJunk updateSpaceJunk
  where
    display = InWindow "Космический мусор" (800, 450) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- | Космический мусор.
data SpaceJunk = SpaceJunk

-- | Сгенерировать космический мусор.
initSpaceJunk :: SpaceJunk
initSpaceJunk = SpaceJunk

-- | Отобразить космический мусор.
drawSpaceJunk :: SpaceJunk -> Picture
drawSpaceJunk _ = blank

-- | Обновить космический мусор.
updateSpaceJunk :: ViewPort -> Float -> SpaceJunk -> SpaceJunk
updateSpaceJunk _ _ = id
