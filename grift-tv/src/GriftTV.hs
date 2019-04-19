{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types
import Data.BitVector.Sized
import Data.Parameterized
import GHC.TypeNats
import GRIFT.Types
import Network.Wai.Handler.Warp
import Servant

-- Orphan instances

instance ToJSON (BitVector w) where
  toJSON (BitVector wRepr x) = object [ "width" .= natValue wRepr
                                      , "value" .= x
                                      ]

instance KnownNat w => FromJSON (BitVector w) where
  parseJSON o = case o of
    Object v -> do
      let wRepr = knownNat @w
      jsonWidth <- v .: "width"
      when (natValue wRepr /= jsonWidth) $
        fail $ "Expected BitVector of width " ++ show (natValue wRepr) ++
                ", got " ++ show jsonWidth
      x :: Integer <- v .: "value"
      return $ bitVector x
    invalid -> typeMismatch "BitVector" invalid

-- Simulation API
type SimAPI = "getReg" :> Get '[JSON] (BitVector (RVWidth RV32I))

-- Server definition
simServer :: Server SimAPI
simServer = return 0x12345678

simAPI :: Proxy SimAPI
simAPI = Proxy

app :: Application
app = serve simAPI simServer

main :: IO ()
main = run 8081 app
  -- let bv = bitVector (0xaa :: Integer) :: BitVector 32
  -- putStrLn $ "bv = " ++ show bv
  -- print (toJSON bv)
  -- print (fromJSON (toJSON bv) :: Result (BitVector 32))
