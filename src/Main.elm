module Main exposing (main)

import Block
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Draggable
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode exposing (Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)


type alias Model =
    { camela : Block.Geo
    , position : ( Int, Int )
    , drag : Draggable.State String
    }


type Msg
    = Delta Float
    | OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg String)


dragConfig : Draggable.Config String Msg
dragConfig =
    Draggable.basicConfig OnDragBy


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
    ( { camela = ( vec3 0 0 10, ( 0, vec3 0 1 0 ) )
      , position = ( 0, 0 )
      , drag = Draggable.init
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( cp, ( crad, caxis ) ) =
            model.camela
    in
    case msg of
        Delta dt ->
            ( -- { model | camela = ( cp, ( crad + dt / 5000, caxis ) ) }
              model
            , Cmd.none
            )

        OnDragBy ( dx, dy ) ->
            let
                ( x, y ) =
                    model.position
            in
            ( { model
                | position = ( round (toFloat x + dx), round (toFloat y + dy) )
                , camela = ( Vec3.add cp (vec3 0 dy 0), ( crad - dx / 10, caxis ) )
              }
            , Cmd.none
            )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta (\dt -> Delta dt)
        , Draggable.subscriptions DragMsg model.drag
        ]


view : Model -> Browser.Document Msg
view model =
    let
        ( cp, ( crad, caxis ) ) =
            model.camela
    in
    { title = "岡田集め - リメイク"
    , body =
        [ Html.div []
            [ Html.text
                (let
                    ( x, y ) =
                        model.position
                 in
                 String.fromInt x ++ ", " ++ String.fromInt y
                )
            ]
        , WebGL.toHtml
            ([ width 600
             , height 600
             , style "display" "block"
             , style "border" "1px solid black"
             , Draggable.mouseTrigger "my-element" DragMsg
             ]
                ++ Draggable.touchTriggers "my-element" DragMsg
            )
            (List.map
                (\( mesh, geo ) ->
                    WebGL.entity
                        vertexShader
                        fragmentShader
                        mesh
                        (uniforms model.camela geo)
                )
                [ ( Block.meshOka, ( vec3 0 0 0, ( 0, vec3 1 0 0 ) ) )
                , ( Block.meshDa, ( vec3 1.1 0 0, ( 0, vec3 1 0 0 ) ) )
                , ( Block.meshOka, ( vec3 2.2 0 0, ( 0, vec3 1 0 0 ) ) )
                , ( Block.meshDa, ( vec3 3.3 0 0, ( 0, vec3 1 0 0 ) ) )
                , ( Block.meshDa, ( vec3 0 1 1.1, ( 0, vec3 0 1 0 ) ) )
                , ( Block.meshOka, ( vec3 1.1 1.1 0, ( 0, vec3 0 1 0 ) ) )
                , ( Block.meshDa, ( vec3 2.2 1.1 0, ( 0, vec3 0 1 0 ) ) )
                , ( Block.meshOka, ( vec3 3.3 1.1 0, ( 0, vec3 0 1 0 ) ) )
                ]
            )
        ]
    }


type alias Uniforms =
    { rotation : Mat4
    , translation : Mat4
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
    { rotation = Mat4.makeRotate t axis
    , translation = Mat4.makeTranslate p
    , perspective = Mat4.makePerspective 45 1 0.01 100
    , camera = Mat4.makeLookAt (Mat4.transform (Mat4.makeRotate ct ca) cp) (vec3 0 0 0) (vec3 0 1 0)
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
