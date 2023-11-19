module Main where

import Graphics.Rendering.OpenGL qualified as GL
import Graphics.Rendering.OpenGL (($=))
import Graphics.UI.GLFW qualified as GLFW
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable (sizeOf)
import Reactive.Banana.Frameworks qualified as RBF

class Drawable a where
    draw :: a -> IO ()

main :: IO ()
main = do
    _ <- GLFW.init
    GLFW.defaultWindowHints
    Just window <- GLFW.createWindow 640 480 "Hello World" Nothing Nothing

    (tickAddHandler, emitTick) <- RBF.newAddHandler

    (windowSizeAddHandler, emitWindowSize) <- RBF.newAddHandler
    GLFW.setWindowSizeCallback window $ Just $
        \_window width height -> emitWindowSize (width, height)

    -- (cursorPosAddHandler, emitCursorPos) <- RBF.newAddHandler
    -- GLFW.setCursorPosCallback window $ Just $
    --     \_window x y -> emitCursorPos (x, y)

    GLFW.makeContextCurrent (Just window)

    program <- buildProgram vertexShader fragmentShader
    GL.currentProgram $= Just program

    identity <- GL.newMatrix GL.ColumnMajor
        [ 1, 0, 0, 0
        , 0, 1, 0, 0
        , 0, 0, 1, 0
        , 0, 0, 0, 1 ] :: IO (GL.GLmatrix GL.GLfloat)

    modelUniform <- GL.get $ GL.uniformLocation program "model"
    viewUniform <- GL.get $ GL.uniformLocation program "view"
    projectionUniform <- GL.get $ GL.uniformLocation program "projection"
    GL.uniform modelUniform $= identity
    GL.uniform viewUniform $= identity
    GL.uniform projectionUniform $= identity

    positionAttribute <- GL.get $ GL.attribLocation program "position"

    colorUniform <- GL.get $ GL.uniformLocation program "color"
    GL.uniform colorUniform $= GL.Color4 (1.0 :: GL.GLfloat) 0.0 0.0 1.0

    let vertices = [ 0.0, 0.5, 0.0
                   , 0.5, -0.5, 0.0
                   , -0.5, -0.5, 0.0 ] :: [Float]

    triangles <- GL.genObjectName
    GL.bindVertexArrayObject $= Just triangles

    buffer <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer $= Just buffer
    withArray vertices $ \ptr -> do
        let size = fromIntegral (length vertices * sizeOf (head vertices))
        GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)

    GL.vertexAttribPointer positionAttribute $=
        (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 (bufferOffset 0))
    GL.vertexAttribArray positionAttribute $= GL.Enabled

    GL.lineWidth $= 1.0

    let lineVerts =
            [ 0.0, 0.0, 0.0
            , 0.5, 0.0, 0.0 ] :: [Float]
    line <- GL.genObjectName
    GL.bindVertexArrayObject $= Just line
    lineBuffer <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer $= Just lineBuffer
    withArray lineVerts $ \ptr -> do
        let size = fromIntegral (length lineVerts * sizeOf (head lineVerts))
        GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
    GL.vertexAttribPointer positionAttribute $=
        (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 (bufferOffset 0))
    GL.vertexAttribArray positionAttribute $= GL.Enabled
    GL.bindVertexArrayObject $= Nothing
    GL.bindBuffer GL.ArrayBuffer $= Nothing

    GL.currentProgram $= Nothing

    polyline <- makePolylineDrawable
        [ 0.0, 0.0, 0.0
        , 0.5, 0.0, 0.0 ]
        (GL.Color4 1.0 0.0 0.0 1.0)
        5.0

    let networkDescription :: RBF.MomentIO ()
        networkDescription = do
            eTick <- RBF.fromAddHandler tickAddHandler
            bWindowSize <- RBF.fromAddHandler windowSizeAddHandler
            RBF.reactimate $
                (\_ -> do
                    GL.clearColor $= GL.Color4 0.3 0.3 0.3 1.0
                    GL.clear [GL.ColorBuffer]
                    -- GL.currentProgram $= Just (_polylineDrawableProgram polyline)
                    -- GL.bindVertexArrayObject $= Just (_polylineDrawableVao polyline)
                    -- GL.drawArrays GL.Lines 0 2)
                    draw polyline)
                    -- GL.currentProgram $= Just program
                    -- GL.bindVertexArrayObject $= Just line
                    -- GL.drawArrays GL.Lines 0 2
                    -- GL.bindVertexArrayObject $= Nothing
                    -- GL.currentProgram $= Nothing)
                <$> eTick
            RBF.reactimate $
                (\(width, height) -> do
                    GL.viewport $=
                        ( GL.Position 0 0
                        , GL.Size
                            (fromIntegral width)
                            (fromIntegral height)))
                <$> bWindowSize

    network <- RBF.compile networkDescription
    RBF.actuate network

    loop window emitTick 0
    GLFW.terminate

cursorPosCallback :: GLFW.CursorPosCallback
cursorPosCallback _window x y = do
    print $ "Cursor position: " ++ show x ++ ", " ++ show y

loop :: GLFW.Window -> (Int -> IO ()) -> Int -> IO ()
loop window emitTick counter = do
    shouldClose <- GLFW.windowShouldClose window
    if shouldClose
        then return ()
        else do
            GLFW.swapBuffers window
            GLFW.pollEvents
            emitTick counter
            loop window emitTick (counter + 1)

data PolylineDrawable = PolylineDrawable
    { _polylineDrawableVao :: GL.VertexArrayObject
    , _polylineDrawableVbo :: GL.BufferObject
    , _polylineDrawableVertexCount :: GL.NumArrayIndices
    , _polylineDrawableColor :: GL.Color4 Float
    , _polylineDrawableLineWidth :: Float
    , _polylineDrawableProgram :: GL.Program
    , _polylineDrawableColorUniform :: GL.UniformLocation }

instance Drawable PolylineDrawable where
    draw polyline = do
        GL.currentProgram $= Just (_polylineDrawableProgram polyline)
        -- GL.lineWidth $= _polylineDrawableLineWidth polyline
        GL.uniform (_polylineDrawableColorUniform polyline) $=
            _polylineDrawableColor polyline
        GL.bindVertexArrayObject $= Just (_polylineDrawableVao polyline)
        GL.drawArrays GL.Lines 0 (_polylineDrawableVertexCount polyline)
        -- GL.currentProgram $= Nothing
        -- GL.bindVertexArrayObject $= Nothing
        -- GL.lineWidth $= 1.0

makePolylineDrawable
    :: [Float]
    -> GL.Color4 Float
    -> Float
    -> IO PolylineDrawable
makePolylineDrawable vertices color lineWidth = do
    program <- buildProgram vertexShader fragmentShader
    GL.currentProgram $= Just program
    positionAttribute <- GL.get $ GL.attribLocation program "position"
    colorUniform <- GL.get $ GL.uniformLocation program "color"
    vao <- GL.genObjectName
    GL.bindVertexArrayObject $= Just vao
    buffer <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer $= Just buffer
    withArray vertices $ \ptr -> do
        let size = fromIntegral (length vertices * sizeOf (head vertices))
        GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)
    GL.vertexAttribPointer positionAttribute $=
        (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 (bufferOffset 0))
    GL.vertexAttribArray positionAttribute $= GL.Enabled
    GL.bindVertexArrayObject $= Nothing
    GL.bindBuffer GL.ArrayBuffer $= Nothing
    GL.currentProgram $= Nothing
    return $ PolylineDrawable
        { _polylineDrawableVao = vao
        , _polylineDrawableVbo = buffer
        , _polylineDrawableVertexCount = fromIntegral (length vertices)
        , _polylineDrawableColor = color
        , _polylineDrawableLineWidth = lineWidth
        , _polylineDrawableProgram = program
        , _polylineDrawableColorUniform = colorUniform }

buildProgram :: String -> String -> IO GL.Program
buildProgram vSource fSource = do
    vs <- buildShader GL.VertexShader vSource
    fs <- buildShader GL.FragmentShader fSource
    program <- GL.createProgram
    GL.attachShader program vs
    GL.attachShader program fs
    GL.linkProgram program
    status <- GL.get $ GL.linkStatus program
    if status
        then do
            GL.detachShader program vs
            GL.detachShader program fs
            GL.deleteObjectName vs
            GL.deleteObjectName fs
            return program
        else do
            print "Program linking failed"
            GL.get (GL.programInfoLog program) >>= print
            error "Program linking failed"

buildShader :: GL.ShaderType -> String -> IO GL.Shader
buildShader shaderType source = do
    shader <- GL.createShader shaderType
    GL.shaderSourceBS shader $= GL.packUtf8 source
    GL.compileShader shader
    status <- GL.get $ GL.compileStatus shader
    if status
        then return shader
        else do
            print "Shader compilation failed"
            GL.get (GL.shaderInfoLog shader) >>= print
            error "Shader compilation failed"

bufferOffset :: Integer -> Ptr a
bufferOffset = plusPtr nullPtr . fromIntegral

vertexShader :: String
vertexShader =
    "#version 120\n\
   \ attribute vec3 position;\n\
   \ uniform mat4 model;\n\
   \ uniform mat4 view;\n\
   \ uniform mat4 projection;\n\
   \ void main()\n\
   \ {\n\
   \    gl_Position = projection * view * model * vec4(position, 1.0f);\n\
   \ }"

fragmentShader :: String
fragmentShader =
    "#version 120\n\
   \ uniform vec4 color;\n\
   \ void main()\n\
   \ {\n\
   \    gl_FragColor = color;\n\
   \ }"
