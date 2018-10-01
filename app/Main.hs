module Main where

import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)

import Servant ( (:>), (:<|>)(..) )

import qualified Servant as S

import qualified Network.Wai.Handler.Warp as W


import Lib

type PingAPI = "ping" :> S.Get '[S.PlainText] String

type BookingsTxtGet = "bookings.txt" :> S.Get '[S.PlainText] String

handlePing :: S.Handler String
handlePing = return "PONG"

handleBookingsTxtGet :: STM.TVar Bookings -> S.Handler String
handleBookingsTxtGet db = liftIO $ do
  v <- STM.atomically $ STM.readTVar db
  return (show v)

api :: S.Proxy (PingAPI :<|> BookingsTxtGet)
api = S.Proxy

server db = handlePing :<|> handleBookingsTxtGet db

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
