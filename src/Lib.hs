module Lib
    ( someFunc
    , mkBooking
    ) where

import qualified Data.Text as T

import Control.Monad (when)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Booking = Booking {
    _start :: Int,
    _end :: Int,
    _description :: T.Text
  } deriving Show

mkBooking :: Int -> Int -> T.Text -> Either String Booking
mkBooking s e d = do

  when (s >= e) $ Left "start must be before end"
  when (s < 0 || s>24) $ Left "start is out of range"
  when (e < 0 || e>24) $ Left "end is out of range"

  Right (Booking s e d) -- Right a.k.a. pure

