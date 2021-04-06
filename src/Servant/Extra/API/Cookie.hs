module Servant.Extra.API.Cookie where

import           Control.Lens
import           Data.OpenApi as OA
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.TypeLits
import           Servant.API as S
import           Servant.OpenApi
import           Servant.OpenApi.Internal

newtype Cookie (name :: Symbol) = Cookie (Maybe Text)
  deriving (Eq, FromHttpApiData, ToHttpApiData)

instance OA.ToParamSchema (Cookie name) where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance HasLink sub => HasLink (Cookie name :> sub) where
  type MkLink (Cookie name :> sub) endpoint = MkLink sub endpoint
  toLink f _p l = toLink f (Proxy @sub) l

instance (HasOpenApi api, KnownSymbol name) =>
  HasOpenApi (Cookie name :> api) where
    toOpenApi _ = toOpenApi (Proxy @api)
      & addParam param
      & addDefaultResponse400 cookieNameText
      where
        cookieNameText = T.pack (symbolVal (Proxy @name))
        param = mempty
          & name .~ cookieNameText
          & in_ .~ OA.ParamCookie
          & required ?~ True
