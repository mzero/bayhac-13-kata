
{-# LANGUAGE OverloadedStrings #-}


module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Route as Route

import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Network.HTTP.Types
import Network.Wai

import Bowling

main :: IO ()
main = Warp.run 9000
        $ Route.dispatch
            [ (Route.rule methodGet "^/display", display)
            ]
        $ respApp notFound


display :: Application
display req = liftIO $ return $ responseLBS status200
                [ ("Content-Type", "text/plain") ]
                (LT.encodeUtf8 $ LT.pack $ resp)
  where

    resp = maybe "\n\nbad input"
            (\bs -> unlines [frameLine bs, scoreLine bs, scoreDisplay bs])
            bowls

    frameLine = frameDisplay2
    scoreLine = intercalate "," . map show . score2
    scoreDisplay = scoreSheet

    bowls = readMaybe $ "[" ++ drop 1 qs ++ "]"
    qs = T.unpack $ T.decodeUtf8 $ rawQueryString req


-- | The missing read function. The string must parse entirely for this to
-- return a value.
readMaybe :: (Read a) => String -> Maybe a
readMaybe = listToMaybe . map fst . filter (null . snd) . reads


-- | 404 \"File not found\"
notFound :: Response
notFound = responseLBS status404
                        [ ("Content-Type", "text/plain") ] "File not found"

-- | 400 \"Bad Request\"
badRequest :: Response
badRequest = responseLBS status400
                        [ ("Content-Type", "text/plain") ] "Bad request"

-- | An application that returns a fixed response no matter the request.
respApp :: Response -> Application
respApp resp _ = liftIO $ return resp
