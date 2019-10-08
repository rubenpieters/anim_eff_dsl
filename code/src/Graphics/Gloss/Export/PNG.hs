module Graphics.Gloss.Export.PNG
    ( exportPictureToPNG
    , exportPicturesToPNG
    ) where

import Codec.Picture.Png (writePng)
import qualified Graphics.Gloss.Rendering as Gloss

import Graphics.Gloss.Export.Image

-- | Save a gloss Picture as PNG
exportPictureToPNG :: Size -- ^ width, height in pixels 
                   -> Gloss.Color -- ^ background color
                   -> FilePath -> Gloss.Picture -> IO ()
exportPictureToPNG  = exportPictureToFormat writePng

-- | Save a gloss animation as PNG
exportPicturesToPNG :: Size        -- ^ width, height in pixels 
                    -> Gloss.Color -- ^ background color
                    -> FilePath
                    -> Animation   -- ^ function that maps from point in time to Picture. analog to Gloss.Animation
                    -> [Float]     -- ^ list of points in time at which to evaluate the animation
                    -> IO ()
exportPicturesToPNG = exportPicturesToFormat writePng

