module SDLM.Main where

import           Control.Monad.Except
import qualified Foreign.C             as C
import           Philed.Data.Vector
import qualified SDL.Raw.Basic         as SDL
import qualified SDL.Raw.Enum          as SDL
import qualified SDL.Raw.Types         as SDL
import qualified SDL.Raw.Video         as SDL
import qualified SDL.TTF.FFI           as TTF

import SDLM.Types

-------------------------------------------------------------------------------------

init :: (MonadIO m, MonadError e m, FromSDLError e) => m ()
init = safeSDL_ (SDL.init SDL.SDL_INIT_VIDEO) >> safeSDL_ (liftIO TTF.init)

quit :: (MonadIO m, MonadError e m, FromSDLError e) => m ()
quit = liftIO TTF.quit >> SDL.quit

createWindow :: (MonadIO m, MonadError e m, FromSDLError e) =>
                Vec Word -> Word -> Word -> m (SDL.Window, SDL.Renderer)
createWindow bottomLeft w h = do
  windowName <- liftIO . C.newCString $ ""
  window     <- safeSDL $ SDL.createWindow windowName
                SDL.SDL_WINDOWPOS_UNDEFINED SDL.SDL_WINDOWPOS_UNDEFINED
                (fromIntegral w) (fromIntegral h)
                SDL.SDL_WINDOW_SHOWN
  renderer   <-
    safeSDL $ SDL.createRenderer window (-1) SDL.SDL_RENDERER_ACCELERATED
  pure (window,renderer)
  where (x,y) = bottomLeft

destroyWindow :: MonadIO m => SDL.Window -> m ()
destroyWindow = SDL.destroyWindow
