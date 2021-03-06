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

import SessionController
import UserController
import ArticleController
import CategoryController
import AboutController

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
      , methodSP GET layout
      , aboutController    tpls
      , sessionController  tpls
      , userController     tpls
      , articleController  tpls
      , categoryController tpls
    ]
    where   layout = renderLayoutSP tpls [("", "")]

getTemplateGroups = directoryGroups "templates"
