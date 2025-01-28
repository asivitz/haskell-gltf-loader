module Text.GLTF.Loader
  ( -- * Scene loading functions
    fromJsonFile,
    fromJsonByteString,
    fromBinaryFile,
    fromBinaryByteString,

    allNamedAnimationJointMatrices,
    animationJointMatrices,

    -- * GLTF Data Types
    module Text.GLTF.Loader.Gltf,
    module Text.GLTF.Loader.Glb,

    -- * Loading Errors
    module Text.GLTF.Loader.Errors
  ) where

import Text.GLTF.Loader.Internal.Adapter
import Text.GLTF.Loader.Internal.BufferAccessor
import Text.GLTF.Loader.Errors
import Text.GLTF.Loader.Glb
import Text.GLTF.Loader.Gltf

import Data.Binary.Get (ByteOffset)
import Data.Either
import Data.Maybe (fromJust)
import Lens.Micro
import RIO
import RIO.FilePath (takeDirectory)
import qualified RIO.Map as Map
import qualified Codec.GlTF as GlTF
import qualified Codec.GLB as GLB
import qualified RIO.Vector as Vector
import Linear (M44, (!*!), identity, V3(..), V4 (..), Quaternion (..), scaled, fromQuaternion, translation, _m33, lerp, slerp)

-- | Load a glTF scene from a ByteString
fromJsonByteString :: MonadUnliftIO io => ByteString -> io (Either Errors Gltf)
fromJsonByteString input = toGltfResult "." Nothing (GlTF.fromByteString input)

-- | Load a glTF scene from a file
fromJsonFile :: MonadUnliftIO io => FilePath -> io (Either Errors Gltf)
fromJsonFile path = liftIO (GlTF.fromFile path) >>= toGltfResult (takeDirectory path) Nothing

fromBinaryFile :: MonadUnliftIO io => FilePath -> io (Either Errors Glb)
fromBinaryFile path = liftIO (GLB.fromFile path) >>= toGlbResult "."

fromBinaryByteString :: MonadUnliftIO io => ByteString -> io (Either Errors Glb)
fromBinaryByteString input = toGlbResult "." (GLB.fromByteString input)

toGltfResult
  :: MonadUnliftIO io
  => FilePath
  -> Maybe GLB.Chunk
  -> Either String GlTF.GlTF
  -> io (Either Errors Gltf)
toGltfResult basePath chunk res = res
  & over _Left (ReadError . fromString)
  & traverseOf _Right (runGltfAdapter basePath chunk)

toGlbResult
  :: MonadUnliftIO io
  => FilePath
  -> Either (ByteOffset, String) GLB.GLB
  -> io (Either Errors Glb)
toGlbResult basePath (Right res) = processGlb basePath res
toGlbResult _        (Left (_, err)) = pure . Left . ReadError . fromString $ err

runGltfAdapter
  :: MonadUnliftIO io
  => FilePath
  -> Maybe GLB.Chunk
  -> GlTF.GlTF
  -> io Gltf
runGltfAdapter basePath chunk gltf = runAdapter gltf
  <$> loadBuffers gltf chunk basePath
  <*> loadImages gltf basePath

processGlb :: MonadUnliftIO io => FilePath -> GLB.GLB -> io (Either Errors Glb)
processGlb basePath GLB.GLB{..} = over _Right Glb <$> res
  where gltfChunk = chunks Vector.!? 0
        bufferChunk = chunks Vector.!? 1
        gltf = GlTF.fromChunk (fromJust gltfChunk)
        res = toGltfResult basePath bufferChunk gltf

data NodeTransform = NodeTransform
  { nodeTranslation :: Maybe (V3 Float)
  , nodeRotation :: Maybe (Quaternion Float)
  , nodeScale :: Maybe (V3 Float)
  }

instance Semigroup NodeTransform where
  (NodeTransform t1 r1 s1) <> (NodeTransform t2 r2 s2) = NodeTransform (t1 <|> t2) (r1 <|> r2) (s1 <|> s2)
instance Monoid NodeTransform where
  mempty = NodeTransform Nothing Nothing Nothing

nodeTransformMatrix ::  NodeTransform -> M44 Float
nodeTransformMatrix NodeTransform {..} =
  tMat !*! rMat !*! sMat
  where
  tMat = maybe identity (\t' -> identity & translation .~ t') nodeTranslation
  rMat = extendm33 $ maybe identity fromQuaternion nodeRotation
  sMat = extendm33 $ maybe identity scaled nodeScale
  extendm33 m = identity & _m33 .~ m

allNamedAnimationJointMatrices :: Gltf -> Int -> Float -> Map.Map Text (Vector (Vector (M44 Float)))
allNamedAnimationJointMatrices gltf skinId frameTime = Map.fromList . Vector.toList
  $ Vector.mapMaybe (\a@Animation {..} -> (,) <$> animationName <*> animationJointMatrices gltf skinId frameTime a) (gltf ^. _animations)

-- | For a given node, animation name, and frame duration (e.g. 1/30), give
-- the joint matrices for the animation. For every frame of animation, we give
-- back a vector of joint matrices (one for each joint).
animationJointMatrices :: Gltf -> Int -> Float -> Animation -> Maybe (Vector (Vector (M44 Float)))
animationJointMatrices Gltf {..} skinId frameTime Animation {..} = do
  someChannelInput <- animationChannels Vector.!? 0 >>= \c -> channelInput c Vector.!? 0
  let animationStart = Vector.foldl' min someChannelInput . Vector.map (Vector.foldl' min someChannelInput . channelInput) $ animationChannels
      animationEnd = Vector.foldl' max 0 . Vector.map (Vector.foldl' max 0 . channelInput) $ animationChannels
      animationLength = animationEnd - animationStart
      frames :: Int = round (animationLength / frameTime)

  Skin {..} <- gltfSkins Vector.!? skinId
  let jointInverseBinds = Vector.zip skinJoints skinInverseBindMatrices

  for [0..frames] $ \frameNumber -> do
    let animatedTargets = Map.fromListWith (<>)
                        . Vector.toList
                        $ (\channel -> (channelTargetNode channel, resolveChannel (animationStart + frameTime * realToFrac frameNumber) channel))
                        <$> animationChannels
    Vector.mapM (jointMatrixForJoint animatedTargets) jointInverseBinds

  where
  resolveChannel :: Float -> Channel -> NodeTransform
  resolveChannel currentTime Channel {..} = case channelOutput of
    TranslationOutput v -> NodeTransform (interp (\a b x -> lerp x b a) v) Nothing Nothing
    RotationOutput v -> NodeTransform Nothing (interp slerp v) Nothing
    ScaleOutput v -> NodeTransform Nothing Nothing (interp (\a b x -> lerp x a b) v)
    where
    toIdx = Vector.findIndex (> currentTime) channelInput
    fromIdx = maybe (Just $ Vector.length channelInput - 1) (\x -> if x > 0 then Just (x - 1) else Nothing) toIdx
    interp :: (a -> a -> Float -> a) -> Vector a -> Maybe a
    interp f v = case (fromIdx, toIdx) of
      (Just i, Nothing) -> v Vector.!? i
      (Nothing, Just i) -> v Vector.!? i
      (Just i, Just j) -> do
        t1 <- channelInput Vector.!? i
        t2 <- channelInput Vector.!? j
        v1 <- v Vector.!? i
        v2 <- v Vector.!? j
        return $ f v1 v2 ((currentTime - t1) / (t2 - t1))
      (Nothing, Nothing) -> Nothing

  -- This global transform is basically a walk from the origin out to the bone
  -- so we need to visit each parent successively
  globalTransformForNode :: Map.Map Int NodeTransform -> Int -> Maybe (M44 Float)
  globalTransformForNode animatedTargets i = do
    let parentXForm = fromMaybe identity $ do
          par <- Map.lookup i parentMap
          globalTransformForNode animatedTargets par
    Node {..} <- gltfNodes Vector.!? i
    let defaultNodeTransform = NodeTransform nodeTranslation (v4toQuat <$> nodeRotation) nodeScale
        nodeTransform = (fromMaybe mempty (Map.lookup i animatedTargets)) <> defaultNodeTransform
        v4toQuat (V4 x y z w) = Quaternion w (V3 x y z)
    return $ parentXForm !*! nodeTransformMatrix nodeTransform

  parentMap :: Map.Map Int Int
  parentMap = Map.fromList
            . Vector.toList
            . Vector.mapMaybe (\(i,_) -> (i,) <$> Vector.findIndex (\node -> i `Vector.elem` nodeChildren node) gltfNodes)
            $ Vector.indexed gltfNodes

  -- The inverse bind matrix will take us to a space where the bone is aligned with the origin
  -- From there we can perform the transform for this bone for this frame of animation
  -- _and_ we'll do this for the bone's parents all the way up to the root, which will take us back
  -- to the full model position
  jointMatrixForJoint :: Map.Map Int NodeTransform -> (Int, M44 Float) -> Maybe (M44 Float)
  jointMatrixForJoint animatedTargets (j, invBindMat) = do
    globalXForm <- globalTransformForNode animatedTargets j
    return $ globalXForm !*! invBindMat
