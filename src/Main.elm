module Main exposing (main)

import Block
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Draggable
import Html
import Html.Attributes exposing (height, style, width)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Shader
import WebGL exposing (Mesh)


type alias Model =
    { camela : Shader.OrbitCamela
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
    ( { camela = ( 10, 0, 0 )
      , position = ( 0, 0 )
      , drag = Draggable.init
      }
    , Cmd.none
    )


limitRadian : Float -> Float
limitRadian r =
    max (min r (pi / 2 * 0.99)) (-pi / 2 * 0.99)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( cr, ca, cb ) =
            model.camela
    in
    case msg of
        Delta _ ->
            ( model
            , Cmd.none
            )

        OnDragBy ( dx, dy ) ->
            ( { model
                | camela = ( cr, ca - dx / 30, limitRadian (cb - dy / 30) )
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
    { title = "岡田集め - リメイク"
    , body =
        [ Html.div []
            [ Html.text
                (let
                    ( cr, ca, cb ) =
                        model.camela
                 in
                 String.fromFloat ca ++ ", " ++ String.fromFloat cb
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
            (entities model.camela
                [ ( Block.meshOka, ( vec3 0 0 0, ( 0, vec3 1 0 0 ) ) )
                , ( Block.meshDa, ( vec3 1.1 0 0, ( 0, vec3 1 0 0 ) ) )
                , ( Block.meshOka, ( vec3 2.2 0 0, ( 0, vec3 1 0 0 ) ) )
                , ( Block.meshDa, ( vec3 3.3 0 0, ( 0, vec3 1 0 0 ) ) )
                , ( Block.meshDa, ( vec3 0 1 0, ( 0, vec3 0 1 0 ) ) )
                , ( Block.meshOka, ( vec3 1.1 1.1 0, ( 0, vec3 0 1 0 ) ) )
                , ( Block.meshDa, ( vec3 2.2 1.1 0, ( 0, vec3 0 1 0 ) ) )
                , ( Block.meshOka, ( vec3 3.3 1.1 1, ( 0, vec3 0 1 0 ) ) )
                ]
            )
        ]
    }


entities : Shader.OrbitCamela -> List ( Mesh Shader.Vertex, Shader.Geo ) -> List WebGL.Entity
entities camela list =
    List.map
        (\( mesh, geo ) ->
            WebGL.entity
                Shader.vertexShader
                Shader.fragmentShader
                mesh
                (Shader.uniforms camela geo)
        )
        list
