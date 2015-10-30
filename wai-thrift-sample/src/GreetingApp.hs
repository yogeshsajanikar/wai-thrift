{-# LANGUAGE OverloadedStrings #-}
module GreetingApp where

import Greeting_Iface as Iface

data GreetData = GreetData


instance Iface.Greeting_Iface GreetData where
  hello _ = return "Hello World!"
