module Databrary.Action.Form
  ( getFormData
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import qualified Network.Wai as Wai
import Network.Wai.Parse (FileInfo)

import Databrary.Has (peeks)
import Databrary.HTTP.Form.Data
import Databrary.HTTP.Parse
import Databrary.Action.Types
import qualified Databrary.JSON as JSON

getFormData :: FileContent a => [(BS.ByteString, Word64)] -> ActionM (FormData a)
getFormData fileLimits = do
  (mkFormData
   :: Map.Map BS.ByteString BS.ByteString -> Maybe JSON.Value -> Map.Map BS.ByteString (FileInfo a) -> FormData a)
    <- peeks $ (\httpReq -> (FormData . Map.fromList . Wai.queryString) httpReq)
  (c :: Content a) <- parseRequestContent getFileMaxSizeByFieldName
  return $ case c of
    ContentForm formParams formFiles -> mkFormData (Map.fromList formParams) Nothing    (Map.fromList formFiles)
    ContentJSON val ->                  mkFormData Map.empty                 (Just val) Map.empty
    -- text or unknown
    _ ->                                mkFormData Map.empty                 Nothing    Map.empty
  where  
    getFileMaxSizeByFieldName :: BS.ByteString -> Word64
    getFileMaxSizeByFieldName fieldName = (fromMaybe 0 . (`lookup` fileLimits)) fieldName
