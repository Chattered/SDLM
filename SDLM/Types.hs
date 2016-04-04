module SDLM.Types where

import           Control.Exception     (IOException)
import           Control.Monad.Except
import           Data.Binary
import           Data.Monoid
import qualified Foreign               as C
import qualified Foreign.C             as C
import qualified SDL.Raw.Error         as SDL
import qualified SDL.Raw.Types as SDL

newtype Colour = Colour SDL.Color deriving (Eq, Show)

rgbaColour :: Word8 -> Word8 -> Word8 -> Word8 -> Colour
rgbaColour r g b a = Colour (SDL.Color r g b a)

instance Ord Colour where
  compare (Colour (SDL.Color r g b a)) (Colour (SDL.Color r' g' b' a')) =
    compare r r' <> compare g g' <> compare b b' <> compare a a'

instance Binary Colour where
  get = Colour <$> (SDL.Color <$> get <*> get <*> get <*> get)
  put (Colour (SDL.Color r g b a)) = put r *> put g *> put b *> put a

class FromSDLError e where
  fromSDLError :: String -> e

instance FromSDLError IOException where
  fromSDLError = userError

throwSDLError :: (MonadError e m, FromSDLError e) => String -> m a
throwSDLError = throwError . fromSDLError

handleSDLError :: (MonadIO m, MonadError e m, FromSDLError e) => m a
handleSDLError = do
  cErrMsg <- liftIO                   SDL.getError
  errMsg  <- liftIO . C.peekCString $ cErrMsg
  throwSDLError errMsg

safeSDL :: (MonadIO m, MonadError e m, FromSDLError e)
           => m (C.Ptr a) -> m (C.Ptr a)
safeSDL m = do
  x <- m
  if x /= C.nullPtr then pure x else handleSDLError

safeSDL_ :: (MonadIO m, MonadError e m, FromSDLError e)
            => m C.CInt -> m ()
safeSDL_ m = do
  result <- m
  unless (result == 0) handleSDLError
