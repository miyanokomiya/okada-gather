module Main exposing (main)

import Block
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode exposing (Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)


type alias Model =
    Float


type Msg
    = Delta Float


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Delta dt ->
            ( model + dt / 5000, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta (\dt -> Delta dt)
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "岡田集め - リメイク"
    , body =
        [ WebGL.toHtml
            [ width 600
            , height 600
            , style "display" "block"
            , style "border" "1px solid black"
            ]
            (entities
                model
                (List.concat
                    [ Block.meshListOka
                        ( vec3 0 0 0, ( 1, vec3 1 0 0 ) )
                    , Block.meshListDa
                        ( vec3 1.1 0 0, ( 1, vec3 1 0 0 ) )
                    ]
                )
             -- [ Block.mesh ( vec3 0 0 0, ( 0, vec3 1 0 0 ) )
             -- , Block.mesh ( vec3 4 0 0, ( 1, vec3 1 0 0 ) )
             -- , Block.mesh ( vec3 -4 0 0, ( 1, vec3 0 1 0 ) )
             -- ]
            )
        ]
    }


entities : Model -> List (Mesh Block.Vertex) -> List WebGL.Entity
entities model meshes =
    List.map
        (\m ->
            WebGL.entity
                vertexShader
                fragmentShader
                m
                (uniforms model)
        )
        meshes


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }


uniforms : Float -> Uniforms
uniforms theta =
    { rotation =
        Mat4.makeRotate (3 * theta) (vec3 0 1 0)
    , perspective = Mat4.makePerspective 45 1 0.01 100
    , camera = Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.8
    }



-- Shaders


vertexShader : Shader Block.Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * rotation * vec4(position, 1.0);
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
