import HAppS.Server
import HAppS.Server.Helpers
import HAppS.State
import Text.StringTemplate
import Text.StringTemplate.Helpers

import Controller
import State.AppState

main = do
    tDirGroups <- getTemplateGroups
    smartserver (Conf 5001 Nothing) "hs-wiki" (controller tDirGroups) 
        stateProxy

stateProxy :: Proxy AppState
stateProxy = Proxy
