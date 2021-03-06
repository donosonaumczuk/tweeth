{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- [Note] This file was extracted from official twitter-conduit samples
-- https://github.com/himura/twitter-conduit/blob/c26a85c5201f6b0d9745ec5a88c8a21084693ae7/sample/common/Common.hs
module Twitter.Utils.Auth where

import           Web.Twitter.Conduit
import           Control.Applicative
import           Control.Lens
import           Network.HTTP.Conduit
import           System.Environment
import qualified Data.ByteString.Char8 as S8
import qualified Data.CaseInsensitive  as CI
import qualified Data.Map              as M
import qualified Network.URI           as URI

getOAuthTokens :: IO (OAuth, Credential)
getOAuthTokens = do
    consumerKey <- getEnv' "OAUTH_CONSUMER_KEY"
    consumerSecret <- getEnv' "OAUTH_CONSUMER_SECRET"
    accessToken <- getEnv' "OAUTH_ACCESS_TOKEN"
    accessSecret <- getEnv' "OAUTH_ACCESS_SECRET"
    let oauth = twitterOAuth
            { oauthConsumerKey = consumerKey
            , oauthConsumerSecret = consumerSecret
            }
        cred = Credential
            [ ("oauth_token", accessToken)
            , ("oauth_token_secret", accessSecret)
            ]
    return (oauth, cred)
  where
    getEnv' = (S8.pack <$>) . getEnv

getProxyEnv :: IO (Maybe Proxy)
getProxyEnv = do
    env <- M.fromList . over (mapped . _1) CI.mk <$> getEnvironment
    let u = M.lookup "https_proxy" env <|>
            M.lookup "http_proxy" env <|>
            M.lookup "proxy" env >>= URI.parseURI >>= URI.uriAuthority
    return $ Proxy <$> (S8.pack . URI.uriRegName <$> u) <*> (parsePort . URI.uriPort <$> u)
  where
    parsePort :: String -> Int
    parsePort []       = 8080
    parsePort (':':xs) = read xs
    parsePort xs       = error $ "port number parse failed " ++ xs

getTWInfoFromEnv :: IO TWInfo
getTWInfoFromEnv = do
    pr <- getProxyEnv
    (oa, cred) <- getOAuthTokens
    return $ (setCredential oa cred def) { twProxy = pr }
