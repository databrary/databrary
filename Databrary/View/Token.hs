{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Token
  ( htmlPasswordToken
  ) where

import Data.Monoid (mempty)

import Databrary.Model.Id
import Databrary.Model.Token
import Databrary.Action
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Token

htmlPasswordToken :: Id LoginToken -> Context -> FormHtml f
htmlPasswordToken tok = htmlForm "Reset Password"
  postPasswordToken (HTML, tok)
  (do
    field "once" inputPassword
    field "again" inputPassword)
  (const mempty)
