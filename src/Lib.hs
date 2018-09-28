module Lib
    ( someFunc
    , mkBooking
    , consBooking
    , overlaps
    ) where

import qualified Data.Text as T

import Control.Monad (when)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Bookings = Bookings [Booking] deriving Show

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

-- c.f. (:) :: Booking -> Bookings -> Bookings 
consBooking :: Booking -> Bookings -> Either String Bookings
consBooking b (Bookings bs) =
  if or (map (overlaps b) bs)
  then Left "new booking overlaps existing booking"
  else pure (Bookings (b : bs))


{-
see https://stackoverflow.com/questions/3269434/whats-the-most-efficient-way-to-test-two-integer-ranges-for-overlap
for discussion of range overlaps
-}

-- checks if two bookings overlap
overlaps :: Booking -> Booking -> Bool
overlaps (Booking l_start l_end _)
         (Booking r_start r_end _) = l_start < r_end && r_start < l_end
