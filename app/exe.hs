{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude (IO, print, (=<<))
import Dropbox
import Servant.Auth.Client

token :: Token
token = Token "sl.AkC0dOjbLWtkw7ikA6RePKDX-2Y-7IELqRX0043qMoX1ubN-q3DJ2BxUFR_JFhJKA56dEeKzenxJtFEunQ_Y3gmzsWOir8DBdRxJa1sIIwNLseJIaJ-xKIWH4HBuSLB3RImwlqY"

main :: IO ()
main = do
  client <- createClient
  print =<< _dropbox_list_folder client token defListFolderRequest
