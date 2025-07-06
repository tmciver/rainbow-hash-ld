module Control.Monad.Error (mapError) where

import           Protolude

mapError
  :: MonadError e' m
  => (e -> e')
  -> ExceptT e m a
  -> m a
mapError f ex = do
  either' <- runExceptT ex
  case either' of
    Left e  -> throwError (f e)
    Right v -> pure v
