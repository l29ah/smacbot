{-# LANGUAGE OverloadedStrings #-}

module Paste (paste) where

import Control.Exception
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Network.HTTP.Client.MultipartFormData

-- |Function to check if text should be pasted
shouldPaste :: Text -> Bool
-- IRC compliance
--shouldPaste text = T.length text > 140 || T.any (== '\n') text
shouldPaste text = T.length text > 500

paste :: Text -> IO Text
paste text = do
    if shouldPaste text
    then catch (pasteToPinnwand text) $ \e -> do
        print (e :: IOException)
        pure text
    else pure text

pasteToPinnwand :: Text -> IO Text
pasteToPinnwand text = do
    let formData = pure $ partBS "raw" $ encodeUtf8 text
    request <- parseRequest "POST https://bpa.st/curl"
    request' <- formDataBody formData $ addRequestHeader "User-Agent" "smacbot" request
    response <- httpBS request'
    pure $ (!! 5) $ T.words $ decodeUtf8 $ responseBody response
