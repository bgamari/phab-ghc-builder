{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Phabricator.Differential
    ( -- * Differential API
      createComment
      -- * Messages
    , Comment(..)
    ) where

import Control.Monad.Trans.Either
import qualified Data.Text as T

import Servant
import Servant.Client
import Phabricator.Types
import Phabricator.Remarkup

data Comment = Comment { cmtBody   :: Maybe Remarkup
                       , cmtAction :: Maybe T.Text -- TODO
                       , cmtSilent :: Bool
                       }

createComment :: BaseUrl -> ApiToken -> Revision -> Comment -> EitherT ServantError IO ()
createComment baseUrl apiToken rev cmt =
    client api baseUrl (Just apiToken) (Just rev)
                                       (cmtBody cmt)
                                       (cmtAction cmt)
                                       (cmtSilent cmt)
  where
    api :: Proxy CreateComment
    api = Proxy

-- | The @differential.createcomment@ endpoint.
type CreateComment = "api" :> "differential.createcomment"
                     :> QueryParam "api.token" ApiToken
                     :> QueryParam "revision_id" Revision
                     :> QueryParam "message" T.Text
                     :> QueryParam "action" T.Text
                     :> QueryFlag "silent"
                     :> Get '[JSON] ()
