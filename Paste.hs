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
shouldPaste text = T.length text > 140 || T.any (== '\n') text

paste :: Text -> IO Text
paste text = do
    if shouldPaste text
    then catch (pasteTo0x0 text) $ \e -> do
        print (e :: IOException)
        pure text
    else pure text

pasteTo0x0 :: Text -> IO Text
pasteTo0x0 text = do
    let formData = pure $ partFileRequestBody "file" "paste" $ RequestBodyBS $ encodeUtf8 text
    request <- parseRequest "POST https://0x0.st"
    request' <- formDataBody formData $ addRequestHeader "User-Agent" "smacbot" request
    response <- httpBS request'
    pure $ T.init $ decodeUtf8 $ responseBody response
