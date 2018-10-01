module Main where

import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)

import Servant ( (:>), (:<|>)(..) )

import qualified Servant as S

import qualified Network.Wai.Handler.Warp as W


import Lib

type PingAPI = "ping" :> S.Get '[S.PlainText] String

type BookingsTxtGet = "bookings.txt" :> S.Get '[S.PlainText] String
type BookingsGet = "bookings" :> S.Get '[S.JSON] Bookings
type BookingGet = "bookings" :> S.Capture "id" Int :> S.Get '[S.JSON] Booking

handlePing :: S.Handler String
handlePing = return "PONG"

handleBookingsTxtGet :: STM.TVar Bookings -> S.Handler String
handleBookingsTxtGet db = liftIO $ do
  v <- STM.atomically $ STM.readTVar db
  return (show v)

handleBookingsGet db = liftIO $ do
  STM.atomically $ STM.readTVar db

handleBookingGet :: STM.TVar Bookings -> Int -> S.Handler Booking
handleBookingGet db s = do
  v <- liftIO $ STM.atomically $ STM.readTVar db
  let booking' = filter (\b -> _start b == s)
                        (bookingsList v)
  case booking' of
    [v] -> return v
    [] -> S.throwError S.err404
    _ -> S.throwError S.err500 -- because something is wrong internally

api :: S.Proxy (PingAPI :<|> BookingsTxtGet :<|> BookingsGet :<|> BookingGet)
api = S.Proxy

server db = handlePing :<|> handleBookingsTxtGet db :<|> handleBookingsGet db :<|> handleBookingGet db

app db = S.serve api (server db)

main :: IO ()
main = do
  db <- mkEmptyBookingDB
  populateSampleDB db
  W.run 8080 (app db)

populateSampleDB db = do
  let (Right r_1) = mkBooking 9 12 "first"
  let (Right r_2) = mkBooking 23 24 "second"
  addBooking db r_1
  addBooking db r_2
