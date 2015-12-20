{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Aeson
import Servant
import Data.Text (Text)

import Build
import Harbormaster

type BuildDiff = "build" :> "diff"
                 :> QueryParam "id" BuildId
                 :> QueryParam "rev" Revision
                 :> QueryParam "diff" Diff
                 :> QueryParam "base_commit" Commit
                 :> QueryParam "phid" Phid
                 :> Post '[JSON] ()

type BuildCommit = "build" :> "commit"
                   :> QueryParam "id" BuildId
                   :> QueryParam "commit" Commit
                   :> QueryParam "phid" Phid
                   :> Post '[JSON] ()

type Api = BuildDiff :<|> BuildCommit

api :: Proxy Api
api = Proxy
