module Databrary.Controller.Zip where

import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Action

zipVolume :: Bool -> ActionRoute (Id Volume)
