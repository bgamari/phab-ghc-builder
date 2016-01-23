{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Servant

import Build
import Phabricator.Types
import Phabricator.Harbormaster

-- | Build a differential.
-- @http://host/build/diff?id=B${build.id}&rev=R${buildable.revision}&diff=D${buildable.diff}&base_commit=${buildable.commit}&phid=${target.phid}@
type BuildDiff = "build" :> "diff"
                 :> QueryParam "id" BuildId
                 :> QueryParam "rev" Revision
                 :> QueryParam "diff" Diff
                 :> QueryParam "base_commit" Commit
                 :> QueryParam "phid" Phid
                 :> Post '[JSON] ()

-- | Build a commit.
-- @http://host/build/commit?id=B${build.id}&commit=${buildable.commit}&phid=${target.phid}@
type BuildCommit = "build" :> "commit"
                   :> QueryParam "id" BuildId
                   :> QueryParam "commit" Commit
                   :> QueryParam "phid" Phid
                   :> Post '[JSON] ()

-- | The build server API
type Api = BuildDiff :<|> BuildCommit

api :: Proxy Api
api = Proxy
