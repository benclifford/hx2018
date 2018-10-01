module Lib
    ( someFunc
    , mkBooking
    , consBooking
    , overlaps
    , mkEmptyBookingDB
    , addBooking
    , Booking()
    , Bookings
    ) where

-- TODO: at some point, comment on these two import styles
-- - specifically how I personally prefer to never import something
-- that you cannot figure out where it was imported from by looking
-- at the import list (so either by name, or by qualifier)
import qualified Data.Text as T
import qualified Control.Concurrent.STM as STM

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

-- We need to specify the type here, because [] is ambigiously typed.
-- If it had an element, that element would imply a type for it.

-- also need to explain why I want to use concurrent - because maybe we'd
-- get several hits in parallel from servant (although figure out if that
-- is the case with default parameters)
mkEmptyBookingDB :: IO (STM.TVar Bookings)
mkEmptyBookingDB = STM.atomically $ STM.newTVar (Bookings []) 

-- return a booking, and its identifier (which for now, we'll use the start
-- time of the booking, but mention we could use something more interesting:
-- for example, start time identifies the booking here at any one instant
-- but if i make a booking, cancel it, and make a new one, that new one will
-- have the same ID.

addBooking :: STM.TVar Bookings -> Booking -> IO (Either String Int)
addBooking db booking = STM.atomically $ do
  oldBookings <- STM.readTVar db
  let newBookings = consBooking booking oldBookings
  case newBookings of
    Right b' -> do
      STM.writeTVar db b'
      pure (Right (_start booking))
    Left err -> pure (Left err)

