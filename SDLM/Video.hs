module SDLM.Video where

import           Control.Monad.Except
import           Data.Bits
import           Data.Function
import           Data.List             (sortBy)
import           Data.Word
import qualified Foreign               as C
import qualified Foreign.C             as C
import           Philed.Data.Vector
import qualified SDL.Raw.Enum          as SDL
import qualified SDL.Raw.Types         as SDL
import qualified SDL.Raw.Video         as SDL
import           SDL.TTF.FFI           (TTFFont)
import qualified SDL.TTF.FFI           as TTF
import           System.ByteOrder

import SDLM.Types

-------------------------------------------------------------------------------------

update :: (MonadIO m, MonadError e m, FromSDLError e) => SDL.Renderer -> m ()
update = SDL.renderPresent

clear :: (MonadIO m, MonadError e m, FromSDLError e) => SDL.Renderer -> m ()
clear = safeSDL_ . SDL.renderClear

-------------------------------------------------------------------------------------

loadTexture :: (MonadIO m, MonadError e m, FromSDLError e)
               => SDL.Renderer -> FilePath -> m SDL.Texture
loadTexture r =
  surfaceToTexture r <=< safeSDL . SDL.loadBMP <=< liftIO . C.newCString

loadFont:: (MonadIO m, MonadError e m, FromSDLError e) =>
           FilePath -> Int -> m (C.Ptr ())
loadFont file ptSize =
  liftIO . C.withCString file $ \cFile ->
  safeSDL (liftIO (TTF.openFont cFile (fromIntegral ptSize)))

stringTexture :: (MonadIO m, MonadError e m, FromSDLError e)
                 => SDL.Renderer -> TTFFont -> String -> Colour
                 -> m SDL.Texture
stringTexture r f str (Colour c) =
  liftIO . C.withCString str $ \cStr ->
  liftIO . C.with c $ \cc ->
  safeSDL (TTF.renderUTF8Solid f cStr cc) >>= surfaceToTexture r

surfaceToTexture :: (MonadIO m, MonadError e m, FromSDLError e)
                    => SDL.Renderer -> C.Ptr SDL.Surface -> m SDL.Texture
surfaceToTexture renderer surface =
  (safeSDL . SDL.createTextureFromSurface renderer $ surface)
    <* SDL.freeSurface surface

rmask :: Word32
gmask :: Word32
bmask :: Word32
amask :: Word32
(rmask,gmask,bmask,amask) =
  case BigEndian of -- TODO: Figure out the correct format for hardware textures
--  case byteOrder of
  BigEndian -> (0xFF000000,0x00FF0000,0x0000FF00,0x000000FF)
  LittleEndian -> (0x000000FF,0x0000FF00,0x00FF0000,0xFF000000)
  Mixed (b1,b2,b3,b4) ->
    let [r,g,b,a] =
          map ((0xFF `shiftL`) . fst) . sortBy (compare `on` snd)
          $ zip [24,16,8,0] [b1,b2,b3,b4] in
    (r,g,b,a)

word8ToWord32 :: Word8 -> Word32
word8ToWord32 n =
  iterate (\m -> m `shiftL` 8 .|. m) n' !! 3
  where n' = fromIntegral n

word32OfRGBA :: Colour -> Word32
word32OfRGBA (Colour (SDL.Color r g b a)) =
  (r' .&. rmask) .|. (g' .&. gmask) .|. (b' .&. bmask) .|. (a' .&. amask)
  where r' = word8ToWord32 r
        g' = word8ToWord32 g
        b' = word8ToWord32 b
        a' = word8ToWord32 a

rectTexture :: (MonadIO m, MonadError e m, FromSDLError e)
               => SDL.Renderer -> Vec Word -> Colour -> m SDL.Texture
rectTexture renderer (w,h) c = do
  let c' = word32OfRGBA c
  sPtr  <- safeSDL $ SDL.createRGBSurface 0 w' h' 32 0 0 0 0
  s     <- liftIO . C.peek $ sPtr
  safeSDL_ . SDL.lockSurface $ sPtr
  let pixelsPtr = C.castPtr . SDL.surfacePixels $ s
  liftIO . flip (iterateNM (fromIntegral $ w'*h')) pixelsPtr $ \p -> do
    C.poke p c'
    return (C.plusPtr p 4)
  SDL.unlockSurface sPtr
  t <- safeSDL (SDL.createTexture renderer SDL.SDL_PIXELFORMAT_RGBA8888
                SDL.SDL_TEXTUREACCESS_STATIC w' h')
  safeSDL_ $ SDL.updateTexture t C.nullPtr (SDL.surfacePixels s) (w'*4)
  safeSDL_ $ SDL.setTextureBlendMode t SDL.SDL_BLENDMODE_BLEND
  return t
  where iterateNM :: Monad m => Int -> (a -> m a) -> a -> m a
        iterateNM 0 f = return
        iterateNM n f = f >=> iterateNM (n-1) f
        w' = fromIntegral w
        h' = fromIntegral h
