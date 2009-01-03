module AboutController
where

import HAppS.Server
import HAppS.State
import Text.StringTemplate
import Text.StringTemplate.Helpers

import State.AppState
import Utils

aboutController :: STDirGroups String -> ServerPartT IO Response
aboutController tpls = multi $ [
        dir "about" [
            method GET $ doShowAbout tpls
        ]
    ]

doShowAbout :: STDirGroups String -> WebT IO Response
doShowAbout tpls = ok . toResponse . HtmlString $ page
    where   page = let a = renderTemplateGroup (template tpls) [("", "")] 
                           "about"
                   in renderLayout tpls [("content", a)]

