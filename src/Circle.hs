-------------------------------------------

-- |
-- Module      : Circle
-- Copyright   : (c) Dylan Martin, 2022
-- Maintainer  : dmarticus@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\developers.circle.com/developer/v1/docs >
--
-- @
-- import Circle.Client
-- import Circle.Types
-- import Network.HTTP.Client (newManager)
-- import Network.HTTP.Client.TLS (tlsManagerSettings)
--
-- main :: IO ()
-- main = do
--   manager <- newManager tlsManagerSettings
--   config <- sandboxEnvConfig \"CIRCLE_API_KEY\"
--   result <- circle config manager getConfigurationInfo
--   case result of
--     Right CircleResponseBody b -> print bs
--     Left CircleError e -> print e
-- @
module Circle
  ( module Circle.Client,
    module Circle.Types,
  )
where

import Circle.Client
import Circle.Types
