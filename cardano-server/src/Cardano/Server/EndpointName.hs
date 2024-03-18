module Cardano.Server.EndpointName where
import           Control.Monad       (when)
import           Control.Monad.Catch (MonadThrow (..))
import           Data.Aeson          (FromJSON (..))
import qualified Data.Aeson          as J
import           Data.Foldable       (toList)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           GHC.TypeLits        (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), KnownSymbol, Symbol, TypeError, symbolVal)
import           Servant             (HasServer (..), Proxy (Proxy), type (:<|>), type (:>))
import           Servant.Client      (HasClient (..))
import           Servant.Client.Core (RunClient)
import           Servant.Server      (err404)

checkThatEndpointIsActive :: forall e api m. (KnownSymbol (GetEndpointName e), MonadThrow m) => EndpointNames api -> m ()
checkThatEndpointIsActive (EndpointNames names) = when (T.pack name `notElem` names) $ throwM err404
    where
        name = symbolVal (Proxy @(GetEndpointName e))

newtype EndpointNames api = EndpointNames [Text]
    deriving newtype (Show, Eq)

instance GetEdpointNames api => FromJSON (EndpointNames api) where
    parseJSON = J.withArray "EndpointNames" $ \arr -> do
        names <- toList <$> mapM (J.withText "EndpointName" pure) arr
        let serverEndpoints = getEndpointNames @api
        case filter (`notElem` serverEndpoints) names of
            [] -> pure $ EndpointNames names
            xs -> fail $ "Server doesn't have these endpoints: " <> show xs
                <> "\nAvailable endpoints: " <> show serverEndpoints

data EndpointWithName (name :: Symbol) a

getEndpointName :: forall e. KnownSymbol (GetEndpointName e) => Text
getEndpointName = T.pack (symbolVal (Proxy @(GetEndpointName e)))

instance HasServer api ctx => HasServer (EndpointWithName name api) ctx where
    type ServerT (EndpointWithName name api) m = ServerT api m
    route Proxy = route (Proxy @api)
    hoistServerWithContext Proxy = hoistServerWithContext (Proxy @api)

instance (RunClient m, HasClient m api) => HasClient m (EndpointWithName name api) where

    type Client m (EndpointWithName name api) = Client m api

    clientWithRoute p Proxy = clientWithRoute p (Proxy @api)

    hoistClientMonad pm _ = hoistClientMonad pm (Proxy @api)

type family GetEndpointName e where
    GetEndpointName ((s :: Symbol) :> b) = s
    GetEndpointName (EndpointWithName name _) = name
    GetEndpointName x = TypeError
        ('Text "Can't infer endpoint name for " ':<>: 'ShowType x ':<>: 'Text "."
        ':$$: 'Text "Wrap " ':<>: 'ShowType x ':<>: 'Text " in " ':<>: 'ShowType EndpointWithName ':<>: 'Text "."
        )

class GetEdpointNames api where
    getEndpointNames :: [Text]

instance {-# OVERLAPPING #-} (GetEdpointNames es, KnownSymbol (GetEndpointName e)) => GetEdpointNames (e :<|> es) where
    getEndpointNames = getEndpointName @e : getEndpointNames @es

instance (KnownSymbol (GetEndpointName e)) => GetEdpointNames e where
    getEndpointNames = [getEndpointName @e]