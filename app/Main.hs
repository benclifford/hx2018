module Main where

import qualified Servant as S
import Servant ( (:>) )

import qualified Network.Wai.Handler.Warp as W

import Lib

type PingAPI = "ping" :> S.Get '[S.PlainText] String

handlePing :: S.Handler String
handlePing = return "PONG"

api :: S.Proxy PingAPI
api = S.Proxy

server = handlePing

app = S.serve api server

main :: IO ()
main = W.run 8080 app
