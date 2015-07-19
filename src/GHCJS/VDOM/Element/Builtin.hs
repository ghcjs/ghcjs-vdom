{-# LANGUAGE TemplateHaskell #-}

module GHCJS.VDOM.Element.Builtin where

import GHCJS.VDOM.Internal

mkElems [ "address", "article", "body", "footer", "header"
        , "h1", "h2", "h3", "h4", "h5", "h6"
        , "hgroup", "nav", "section"
        , "dd", "div", "dl", "dt", "figcaption", "figure", "hr", "li"
        , "main", "ol", "p", "pre", "ul"
        , "a", "abbr", "b", "bdi", "br", "cite", "code", "dfn"
        , "em", "i", "kbd", "mark", "q", "rp", "rt", "rtc", "ruby"
        , "s", "samp", "small", "span", "strong", "sub", "sup", "time"
        , "u", "var", "wbr"
        , "area", "audio", "img", "map", "track", "video"
        , "embed", "iframe", "object", "param", "source"
        , "canvas", "noscript", "script"
        , "del", "ins"
        , "caption", "col", "colgroup", "table", "tbody", "td", "tfoot"
        , "th", "thead", "tr"
        , "button", "datalist", "fieldset", "form", "input", "keygen"
        , "label", "legend", "meter", "optgroup", "option", "output"
        , "progress", "select", "textarea"
        , "details", "dialog", "menu", "menuitem", "summary"
        , "content", "element", "shadow", "template"
        ]

-- use 'data_' as the name for the data tag, data is a reserved word
mkElem "data_" "data"
