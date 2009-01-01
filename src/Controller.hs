{-# LANGUAGE NoMonomorphismRestriction #-}
module Controller 
    ( controller
    , getTemplateGroups
    )
where

import HAppS.Server
import HAppS.State
import Text.StringTemplate
import Text.StringTemplate.Helpers

import State.AppState
import Utils

staticFiles :: ServerPartT IO Response
staticFiles = multi [
        staticserve "images"
      , staticserve "javascript"
      , staticserve "stylesheets"
    ]
    where staticserve d = dir d [ fileServe [] ("public/" ++ d) ]

controller :: STDirGroups String -> [ServerPartT IO Response]
controller tpls = [
        staticFiles
      , method GET $ ok (toResponse . HtmlString $ layout)
    ]
    where   layout = renderLayout tpls [("", "")]

getTemplateGroups = directoryGroups "templates"
