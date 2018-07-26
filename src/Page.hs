{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Page where

import Lucid

page :: Html ()
page = h1_ [class_ "heading"] "Website template page"
