{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
-- src/LLMCoder.hs
module LLMCoder (
    runCoderAgent,
    LLMConfig(..),
    getLLMConfig,
    extractPythonCode
) where

import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Monad.IO.Class (MonadIO(..))
import Network.HTTP.Req
import System.Environment (lookupEnv)
import Text.Printf (printf)
import qualified Data.Vector as V
import Control.Exception (try)

-- Configuration
data LLMConfig = LLMConfig {
    modelName :: T.Text,
    apiEndpoint :: Url 'Https,
    apiKey :: T.Text
}

-- Helper to extract Python code block
extractPythonCode :: T.Text -> Maybe T.Text
extractPythonCode response =
    let ls = T.lines response
        startMarker = "```python"
        endMarker = "```"
        -- Find the start marker, drop it, then take lines until the end marker
        codeLines = takeWhile (/= endMarker) $ drop 1 $ dropWhile (not . T.isPrefixOf startMarker) ls
    in if null codeLines && not (any (T.isPrefixOf startMarker) ls)
       then Nothing 
       else Just (T.unlines codeLines)

-- Initialization
getLLMConfig :: IO (Maybe LLMConfig)
getLLMConfig = do
    apiKeyEnv <- lookupEnv "OPENAI_API_KEY"
    case apiKeyEnv of
        Nothing -> return Nothing
        Just key -> do
            let endpoint = https "api.openai.com" /: "v1" /: "chat" /: "completions"
            let model = "gpt-4o"  -- Change this to your preferred model
            printf "Initialized LLM Config (Model: %s)\n" model
            return $ Just $ LLMConfig (T.pack model) endpoint (T.pack key)

-- API Call
runCoderAgent :: LLMConfig -> T.Text -> IO (Either String T.Text)
runCoderAgent config prompt = do
    result <- try $ runReq defaultHttpConfig $ do
        let payload = object [
                "model" .= modelName config,
                "messages" .= [
                    object ["role" .= ("user" :: T.Text), "content" .= prompt]
                ],
                "temperature" .= (0.5 :: Double)
              ]

        let authHeader = oAuth2Bearer (TE.encodeUtf8 $ apiKey config)

        -- Make the POST request
        response <- req POST (apiEndpoint config) (ReqBodyJson payload) jsonResponse (authHeader <> header "Content-Type" "application/json")
        
        let body = responseBody response :: Value
        -- Extract the content from the response JSON
        case extractContent body of
            Just content -> return content
            Nothing -> error $ "Failed to parse LLM response: " ++ show body
    
    case result of
        -- Handle network exceptions (HttpException)
        Left (e :: HttpException) -> return $ Left $ "Network or API error: " ++ show e
        Right content -> return $ Right content

-- JSON Parsing Helper
extractContent :: Value -> Maybe T.Text
extractContent (Object obj) = do
    choicesVal <- KM.lookup (K.fromText "choices") obj
    case choicesVal of
        (Array choices) -> do
            firstChoice <- choices V.!? 0
            case firstChoice of
                (Object choiceObj) -> do
                    messageVal <- KM.lookup (K.fromText "message") choiceObj
                    case messageVal of
                        (Object msgObj) -> do
                            contentVal <- KM.lookup (K.fromText "content") msgObj
                            case contentVal of
                                (String content) -> Just content
                                _ -> Nothing
                        _ -> Nothing
                _ -> Nothing
        _ -> Nothing
extractContent _ = Nothing