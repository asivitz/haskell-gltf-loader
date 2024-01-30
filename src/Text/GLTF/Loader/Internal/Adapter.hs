-- |Transform a `Codec.GlTF.GlTF` to `Text.GLTF.Loader.Gltf.Gltf`
module Text.GLTF.Loader.Internal.Adapter
  ( attributePosition,
    attributeNormal,
    attributeTangent,
    attributeTexCoord,
    runAdapter,
    adaptGltf,
    adaptAsset,
    adaptImages,
    adaptMaterials,
    adaptMeshes,
    adaptNodes,
    adaptTextures,
    adaptImage,
    adaptMaterial,
    adaptMesh,
    adaptNode,
    adaptTexture,
    adaptAlphaMode,
    adaptPbrMetallicRoughness,
    adaptMeshPrimitives,
    adaptMeshPrimitive,
    adaptMeshPrimitiveMode
  ) where

import Text.GLTF.Loader.Gltf
import Text.GLTF.Loader.Internal.BufferAccessor
import Text.GLTF.Loader.Internal.MonadAdapter

import Linear (V3(..), V4(..), Quaternion (..))
import RIO
import RIO.Partial (toEnum)
import RIO.Vector.Partial ((!))
import qualified Codec.GlTF as GlTF
import qualified Codec.GlTF.Asset as Asset
import qualified Codec.GlTF.Image as Image
import qualified Codec.GlTF.Material as Material
import qualified Codec.GlTF.PbrMetallicRoughness as PbrMetallicRoughness
import qualified Codec.GlTF.Mesh as Mesh
import qualified Codec.GlTF.Node as Node
import qualified Codec.GlTF.Sampler as Sampler
import qualified Codec.GlTF.Texture as Texture
import qualified Codec.GlTF.TextureInfo as TextureInfo
import qualified Codec.GlTF.Skin as Skin
import qualified Codec.GlTF.Animation as Animation
import qualified Data.HashMap.Strict as HashMap
import qualified RIO.Vector as V
import Codec.GlTF.Accessor (AccessorIx)
import Lens.Micro.Aeson (key, values, _String)

attributePosition :: Text
attributePosition = "POSITION"

attributeNormal :: Text
attributeNormal = "NORMAL"

attributeTexCoord :: Text
attributeTexCoord = "TEXCOORD_0"

attributeJoints :: Text
attributeJoints = "JOINTS_0"

attributeWeights :: Text
attributeWeights = "WEIGHTS_0"

attributeTangent :: Text
attributeTangent = "TANGENT"

runAdapter
  :: GlTF.GlTF
  -> Vector GltfBuffer
  -> Vector GltfImageData
  -> Gltf
runAdapter gltf buffers images = runReader adaptGltf env
  where env = AdaptEnv gltf buffers images

adaptGltf :: Adapter Gltf
adaptGltf = do
  GlTF.GlTF{..} <- getGltf

  gltfImages <- adaptImages images
  gltfMeshes <- adaptMeshes meshes
  gltfSkins  <- adaptSkins skins
  gltfAnimations  <- adaptAnimations animations

  return $ Gltf
    { gltfAsset = adaptAsset asset,
      gltfImages = gltfImages,
      gltfMaterials = adaptMaterials materials,
      gltfMeshes = gltfMeshes,
      gltfNodes = adaptNodes nodes,
      gltfSamplers = adaptSamplers samplers,
      gltfTextures = adaptTextures textures,
      gltfSkins = gltfSkins,
      gltfAnimations = gltfAnimations
    }

adaptAsset :: Asset.Asset -> Asset
adaptAsset Asset.Asset{..} = Asset
  { assetVersion = version,
    assetCopyright = copyright,
    assetGenerator = generator,
    assetMinVersion = minVersion
  }

adaptImages :: Maybe (Vector Image.Image) -> Adapter (Vector Image)
adaptImages codecImages = do
  imageData <- getImages

  let images' = fromMaybe mempty codecImages
      iforM = flip V.imapM

  iforM images' $ \imgId img ->
    adaptImage (imageData ! imgId) img

adaptMaterials :: Maybe (Vector Material.Material) -> Vector Material
adaptMaterials = maybe mempty (fmap adaptMaterial)

adaptMeshes :: Maybe (Vector Mesh.Mesh) -> Adapter (Vector Mesh)
adaptMeshes = maybe (return mempty) (V.mapM adaptMesh)

adaptNodes :: Maybe (Vector Node.Node) -> Vector Node
adaptNodes = maybe mempty (fmap adaptNode)

adaptSamplers :: Maybe (Vector Sampler.Sampler) -> Vector Sampler
adaptSamplers = maybe mempty (fmap adaptSampler)

adaptTextures :: Maybe (Vector Texture.Texture) -> Vector Texture
adaptTextures = maybe mempty (fmap adaptTexture)

adaptSkins :: Maybe (Vector Skin.Skin) -> Adapter (Vector Skin)
adaptSkins = maybe (return mempty) (V.mapM adaptSkin)

adaptAnimations :: Maybe (Vector Animation.Animation) -> Adapter (Vector Animation)
adaptAnimations = maybe (return mempty) (V.mapM adaptAnimation)

adaptImage :: GltfImageData -> Image.Image -> Adapter Image
adaptImage imgData Image.Image{..} = do
  payload <- getImageData imgData

  -- Note that we treat mimeType as required, even though it may not be in the
  -- specification. Tests in Blender suggest it's ALWAYS provided; When we come
  -- across an example where it isn't, we'll address it then.
  case mimeType of
    Nothing -> error "Invalid Image: no mime-type specified"
    Just mimeType' -> return Image
      { imageData = payload,
        imageMimeType = mimeType',
        imageName = name
      }

adaptMaterial :: Material.Material -> Material
adaptMaterial Material.Material{..} = Material
  { materialAlphaCutoff = alphaCutoff,
    materialAlphaMode = adaptAlphaMode alphaMode,
    materialDoubleSided = doubleSided,
    materialEmissiveFactor = toV3 emissiveFactor,
    materialName = name,
    materialPbrMetallicRoughness = adaptPbrMetallicRoughness <$> pbrMetallicRoughness
  }

adaptMesh :: Mesh.Mesh -> Adapter Mesh
adaptMesh Mesh.Mesh{..} = do
  primitives' <- adaptMeshPrimitives primitives

  return $ Mesh
    { meshPrimitives = primitives',
      meshWeights = fromMaybe mempty weights,
      meshName = name,
      meshTargetNames = V.fromList $ extras ^.. traverse . key "targetNames" . values . _String
    }

adaptNode :: Node.Node -> Node
adaptNode Node.Node{..} = Node
  { nodeChildren = maybe mempty (fmap Node.unNodeIx) children,
    nodeMeshId = Mesh.unMeshIx <$> mesh,
    nodeName = name,
    nodeRotation = toV4 <$> rotation,
    nodeScale = toV3 <$> scale,
    nodeTranslation = toV3 <$> translation,
    nodeWeights = maybe [] toList weights,
    nodeSkinId = Skin.unSkinIx <$> skin
  }

adaptSampler :: Sampler.Sampler -> Sampler
adaptSampler Sampler.Sampler{..} = Sampler
  { samplerMagFilter = adaptMagFilter <$> magFilter,
    samplerMinFilter = adaptMinFilter <$> minFilter,
    samplerName = name,
    samplerWrapS = adaptSamplerWrap wrapS,
    samplerWrapT = adaptSamplerWrap wrapT
  }

adaptTexture :: Texture.Texture -> Texture
adaptTexture Texture.Texture{..} = Texture
  { textureName = name,
    textureSamplerId = Sampler.unSamplerIx <$> sampler,
    textureSourceId = Image.unImageIx <$> source
  }

adaptSkin :: Skin.Skin -> Adapter Skin
adaptSkin Skin.Skin{..} = do
  gltf <- getGltf
  buffers' <- getBuffers

  return $ Skin
    { skinInverseBindMatrices = maybe mempty (decodeSkinInverseBindMatrices gltf buffers') inverseBindMatrices,
      skinJoints = Node.unNodeIx <$> joints
    }

adaptAnimation :: Animation.Animation -> Adapter Animation
adaptAnimation Animation.Animation{..} = do
  gltf <- getGltf
  buffers' <- getBuffers

  return $ Animation
    { animationName = name,
      animationChannels = V.mapMaybe (adaptAnimationChannel gltf buffers' samplers) channels
    }

adaptAnimationChannel :: GlTF.GlTF -> Vector GltfBuffer -> Vector Animation.AnimationSampler -> Animation.AnimationChannel -> Maybe Channel
adaptAnimationChannel gltf buffers samplers Animation.AnimationChannel {..} = do
  let Animation.AnimationChannelTarget { node, path } = target

  Animation.AnimationSampler { input, output, interpolation } <- samplers V.!? Animation.unAnimationSamplerIx sampler
  node' <- node

  output' <- case Animation.unAnimationChannelTargetPath path of
    "translation" -> Just $ TranslationOutput (decodeSamplerTranslations gltf buffers output)
    "rotation" -> Just $ RotationOutput (V.map (\(V4 x y z w) -> Quaternion w (V3 x y z)) $ decodeSamplerRotations gltf buffers output)
    "scale" -> Just $ ScaleOutput (decodeSamplerScales gltf buffers output)
    _ -> Nothing

  return $ Channel
    { channelTargetNode = Node.unNodeIx node'
    , channelInterpolation = Animation.unAnimationSamplerInterpolation interpolation
    , channelInput = decodeSamplerInput gltf buffers input
    , channelOutput = output'
    }

getImageData :: GltfImageData -> Adapter (Maybe ByteString)
getImageData (ImageData payload) = return $ Just payload
getImageData NoImageData = return Nothing
getImageData (ImageBufferView bufferViewId) = imageDataRaw' <$> getGltf <*> getBuffers
  where imageDataRaw' gltf buffers' = imageDataRaw gltf buffers' bufferViewId

adaptAlphaMode :: Material.MaterialAlphaMode -> MaterialAlphaMode
adaptAlphaMode Material.BLEND = Blend
adaptAlphaMode Material.MASK = Mask
adaptAlphaMode Material.OPAQUE = Opaque
adaptAlphaMode (Material.MaterialAlphaMode alphaMode)
  = error $ "Invalid MaterialAlphaMode: " <> show alphaMode

adaptPbrMetallicRoughness
  :: PbrMetallicRoughness.PbrMetallicRoughness
  -> PbrMetallicRoughness
adaptPbrMetallicRoughness PbrMetallicRoughness.PbrMetallicRoughness{..}
  = PbrMetallicRoughness
    { pbrBaseColorFactor = toV4 baseColorFactor,
      pbrBaseColorTexture = adaptTextureInfo <$> baseColorTexture,
      pbrMetallicFactor = metallicFactor,
      pbrRoughnessFactor = roughnessFactor
    }

adaptMeshPrimitives :: Vector Mesh.MeshPrimitive -> Adapter (Vector MeshPrimitive)
adaptMeshPrimitives = V.mapM adaptMeshPrimitive

adaptMagFilter :: Sampler.SamplerMagFilter -> MagFilter
adaptMagFilter Sampler.MAG_LINEAR = MagLinear
adaptMagFilter Sampler.MAG_NEAREST = MagNearest
adaptMagFilter mode = error $ "Invalid MagFilter: " <> show mode

adaptMinFilter :: Sampler.SamplerMinFilter -> MinFilter
adaptMinFilter Sampler.MIN_NEAREST = MinNearest
adaptMinFilter Sampler.MIN_LINEAR = MinLinear
adaptMinFilter Sampler.MIN_NEAREST_MIPMAP_NEAREST = MinNearestMipmapNearest
adaptMinFilter Sampler.MIN_NEAREST_MIPMAP_LINEAR = MinNearestMipmapLinear
adaptMinFilter Sampler.MIN_LINEAR_MIPMAP_NEAREST = MinLinearMipmapNearest
adaptMinFilter Sampler.MIN_LINEAR_MIPMAP_LINEAR = MinLinearMipmapLinear
adaptMinFilter mode = error $ "Invalid MinFilter: " <> show mode

adaptSamplerWrap :: Sampler.SamplerWrap -> SamplerWrap
adaptSamplerWrap Sampler.CLAMP_TO_EDGE = ClampToEdge
adaptSamplerWrap Sampler.MIRRORED_REPEAT = MirroredRepeat
adaptSamplerWrap Sampler.REPEAT = Repeat
adaptSamplerWrap mode = error $ "Invalid SamplerWrap: " <> show mode

adaptTextureInfo :: TextureInfo.TextureInfo a -> TextureInfo
adaptTextureInfo TextureInfo.TextureInfo{..} = TextureInfo
  { textureId = index,
    textureTexCoord = texCoord
  }

adaptMeshPrimitive :: Mesh.MeshPrimitive -> Adapter MeshPrimitive
adaptMeshPrimitive Mesh.MeshPrimitive{..} = do
  gltf <- getGltf
  buffers' <- getBuffers
  targets' <- V.mapM adaptMorphTarget (fromMaybe mempty targets)

  return $ MeshPrimitive
    { meshPrimitiveIndices = maybe mempty (vertexIndices gltf buffers') indices,
      meshPrimitiveMaterial = Material.unMaterialIx <$> material,
      meshPrimitiveMode = adaptMeshPrimitiveMode mode,
      meshPrimitiveNormals = maybe mempty (vertexNormals gltf buffers') normals,
      meshPrimitiveTangents = maybe mempty (vertexTangents gltf buffers') tangents,
      meshPrimitivePositions = maybe mempty (vertexPositions gltf buffers') positions,
      meshPrimitiveTexCoords = maybe mempty (vertexTexCoords gltf buffers') texCoords,
      meshPrimitiveJoints = maybe mempty (vertexJoints gltf buffers') joints,
      meshPrimitiveWeights = maybe mempty (vertexWeights gltf buffers') weights,
      meshPrimitiveTargets = targets'
    }
    where positions = attributes HashMap.!? attributePosition
          normals = attributes HashMap.!? attributeNormal
          tangents = attributes HashMap.!? attributeTangent
          texCoords = attributes HashMap.!? attributeTexCoord
          joints = attributes HashMap.!? attributeJoints
          weights = attributes HashMap.!? attributeWeights

adaptMorphTarget :: HashMap Text AccessorIx -> Adapter MorphTarget
adaptMorphTarget attributes = do
  gltf <- getGltf
  buffers' <- getBuffers

  return $ MorphTarget
    { morphTargetNormals = maybe mempty (vertexNormals gltf buffers') normals,
      morphTargetTangents = maybe mempty (vertexTangents gltf buffers') tangents,
      morphTargetPositions = maybe mempty (vertexPositions gltf buffers') positions,
      morphTargetTexCoords = maybe mempty (vertexTexCoords gltf buffers') texCoords,
      morphTargetJoints = maybe mempty (vertexJoints gltf buffers') joints,
      morphTargetWeights = maybe mempty (vertexWeights gltf buffers') weights
    }
    where positions = attributes HashMap.!? attributePosition
          normals = attributes HashMap.!? attributeNormal
          tangents = attributes HashMap.!? attributeTangent
          texCoords = attributes HashMap.!? attributeTexCoord
          joints = attributes HashMap.!? attributeJoints
          weights = attributes HashMap.!? attributeWeights

adaptMeshPrimitiveMode :: Mesh.MeshPrimitiveMode -> MeshPrimitiveMode
adaptMeshPrimitiveMode = toEnum . Mesh.unMeshPrimitiveMode

toV3 :: (a, a, a) -> V3 a
toV3 (x, y, z) = V3 x y z

toV4 :: (a, a, a, a) -> V4 a
toV4 (w, x, y, z) = V4 w x y z
