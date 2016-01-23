module Rackspace.Server where

import Rackspace.Server.Create
import Rackspace.Server.Delete
import Rackspace.Server.Get
import Servant.Client

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "servers.api.rackspacecloud.com" 80
