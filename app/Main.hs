module Main where

import SpaceJunk

main :: IO ()
main = do
  images <- loadImages
  demo images
