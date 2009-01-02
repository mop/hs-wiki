module $name$
where

import HAppS.Server
import HAppS.State
import Text.StringTemplate
import Text.StringTemplate.Helpers

import State.AppState
import Utils

$smallname$ :: STDirGroups String -> ServerPartT IO Response
$smallname$ tpls = multi \$ [
        dir "dir1" [
            methodSP GET \$ action1 tpls
          , methodSP POST \$ action2 tpls
        ]
    ]

action1 :: STDirGroups String -> ServerPartT IO Response
action1 tpls = error "undefined"

action2 :: STDirGroups String -> ServerPartT IO Response
action2 tpls = error "undefined"

