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
    { camela : Block.Geo
    }


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
    ( { camela = ( vec3 0 0 10, ( 0, vec3 0 1 0 ) ) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( cp, ( crad, caxis ) ) =
            model.camela
    in
    case msg of
        Delta dt ->
            ( { model | camela = ( cp, ( crad + dt / 5000, caxis ) ) }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta (\dt -> Delta dt)
        ]


view : Model -> Browser.Document Msg
view model =
    let
        ( cp, ( crad, caxis ) ) =
            model.camela
    in
    { title = "岡田集め - リメイク"
    , body =
        [ WebGL.toHtml
            [ width 600
            , height 600
            , style "display" "block"
            , style "border" "1px solid black"
            ]
            (List.map
                (\( mesh, geo ) ->
                    WebGL.entity
                        vertexShader
                        fragmentShader
                        mesh
                        (uniforms model.camela geo)
                )
                [ ( Block.meshOka, ( vec3 0 0 0, ( crad, vec3 1 0 0 ) ) )
                , ( Block.meshDa, ( vec3 1.1 0 0, ( crad, vec3 1 0 0 ) ) )
                , ( Block.meshOka, ( vec3 2.2 0 0, ( crad, vec3 1 0 0 ) ) )
                , ( Block.meshDa, ( vec3 3.3 0 0, ( crad, vec3 1 0 0 ) ) )
                , ( Block.meshDa, ( vec3 0 1 1.1, ( crad, vec3 0 1 0 ) ) )
                , ( Block.meshOka, ( vec3 1.1 1.1 0, ( crad, vec3 0 1 0 ) ) )
                , ( Block.meshDa, ( vec3 2.2 1.1 0, ( crad, vec3 0 1 0 ) ) )
                , ( Block.meshOka, ( vec3 3.3 1.1 0, ( crad, vec3 0 1 0 ) ) )
                ]
            )
        ]
    }


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }


uniforms : Block.Geo -> Block.Geo -> Uniforms
uniforms ( cp, ( ct, ca ) ) ( position, ( t, axis ) ) =
    let
        p =
            Vec3.scale -1 position
    in
    { rotation =
        Mat4.mul
            (Mat4.makeRotate ct ca)
            (Mat4.makeRotate t axis)
    , perspective = Mat4.makePerspective 45 1 0.01 100
    , camera = Mat4.makeLookAt (Vec3.add cp p) p (vec3 0 1 0)
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
