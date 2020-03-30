module Shader exposing
    ( Geo
    , OrbitCamela
    , Vertex
    , fragmentShader
    , uniforms
    , vertexShader
    )

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Shader)


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


type alias Rotation =
    ( Float, Vec3 )


type alias Geo =
    ( Vec3, Rotation )


{-| (radius, radianA, radianB)
-}
type alias OrbitCamela =
    ( Float, Float, Float )


type alias Uniforms =
    { rotation : Mat4
    , translation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }


uniforms : OrbitCamela -> Geo -> Uniforms
uniforms camela ( position, ( t, axis ) ) =
    { rotation = Mat4.makeRotate t axis
    , translation = Mat4.makeTranslate position
    , perspective = Mat4.makePerspective 45 1 0.01 100
    , camera = Mat4.makeLookAt (orbitCamelaPosition camela) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.8
    }


orbitCamelaPosition : OrbitCamela -> Vec3
orbitCamelaPosition ( cr, ca, cb ) =
    Mat4.transform
        (Mat4.mul
            (Mat4.makeRotate ca (vec3 0 1 0))
            (Mat4.makeRotate cb (vec3 1 0 0))
        )
        (vec3 0 0 cr)



-- Shaders


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        uniform mat4 translation;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * rotation * translation * vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        uniform float shade;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = shade * vec4(vcolor, 1.0);
        }

    |]
