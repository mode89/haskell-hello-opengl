module Main where

import Graphics.Rendering.OpenGL qualified as GL
import Graphics.Rendering.OpenGL (($=))
import Graphics.UI.GLFW qualified as GLFW
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable (sizeOf)

main :: IO ()
main = do
    _ <- GLFW.init
    GLFW.defaultWindowHints
    Just window <- GLFW.createWindow 640 480 "Hello World" Nothing Nothing
    GLFW.makeContextCurrent (Just window)

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

    GL.vertexAttribPointer (GL.AttribLocation 0) $=
        (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 (bufferOffset 0))
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

    program <- buildProgram vertexShader fragmentShader
    GL.currentProgram $= Just program

    loop window triangles
    GLFW.terminate

loop :: GLFW.Window -> GL.VertexArrayObject -> IO ()
loop win vao = do
    shouldClose <- GLFW.windowShouldClose win
    if shouldClose
        then return ()
        else do
            GLFW.swapBuffers win
            GLFW.pollEvents
            draw vao
            loop win vao

draw :: GL.VertexArrayObject -> IO ()
draw vao = do
    GL.clearColor $= GL.Color4 0.3 0.3 0.3 1.0
    GL.clear [GL.ColorBuffer]
    GL.bindVertexArrayObject $= Just vao
    GL.drawArrays GL.Triangles 0 3

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
            putStrLn "Program linking failed"
            GL.get (GL.programInfoLog program) >>= putStrLn
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
            putStrLn "Shader compilation failed"
            GL.get (GL.shaderInfoLog shader) >>= putStrLn
            error "Shader compilation failed"

bufferOffset :: Integer -> Ptr a
bufferOffset = plusPtr nullPtr . fromIntegral

vertexShader :: String
vertexShader =
    "#version 450\n\
   \ layout (location = 0) in vec3 position;\n\
   \ void main()\n\
   \ {\n\
   \    gl_Position = vec4(position.x, position.y, position.z, 1.0);\n\
   \ }"

fragmentShader :: String
fragmentShader =
    "#version 450\n\
   \ out vec4 color;\n\
   \ void main()\n\
   \ {\n\
   \    color = vec4(1.0f, 1.0f, 1.0f, 1.0f);\n\
   \ }"
