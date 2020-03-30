module Shader exposing
    ( Camela
    , Geo
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


type alias Camela =
    Geo


type alias Uniforms =
    { rotation : Mat4
    , translation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }


uniforms : Camela -> Geo -> Uniforms
uniforms ( cp, ( ct, ca ) ) ( position, ( t, axis ) ) =
    { rotation = Mat4.makeRotate t axis
    , translation = Mat4.makeTranslate position
    , perspective = Mat4.makePerspective 45 1 0.01 100
    , camera = Mat4.makeLookAt (Mat4.transform (Mat4.makeRotate ct ca) cp) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.8
    }



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
