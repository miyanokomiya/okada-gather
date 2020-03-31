module Main exposing (main)

import Asset
import Block
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Draggable
import Html
import Html.Attributes exposing (height, style, width)
import Html.Events.Extra.Mouse as Mouse
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Shader
import WebGL exposing (Mesh)


type Okada
    = Oka
    | Da


type alias BlockWithGeo =
    ( Okada, Shader.Geo )


type alias Model =
    { size : ( Int, Int )
    , camera : Shader.OrbitCamela
    , position : ( Float, Float )
    , drag : Draggable.State String
    , blocks : List BlockWithGeo
    , selected : Maybe BlockWithGeo
    }


type Msg
    = Delta Float
    | OnDragBy Draggable.Delta
    | DragMsg (Draggable.Msg String)
    | ClickMsg ( Float, Float )


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
    ( { size = ( 800, 600 )
      , camera = ( 10, 0, 0 )
      , position = ( 0, 0 )
      , drag = Draggable.init
      , blocks =
            [ ( Oka, ( vec3 0 0 0, ( 0, vec3 1 0 0 ) ) )
            , ( Da, ( vec3 1.1 0 0, ( 0, vec3 1 0 0 ) ) )
            , ( Oka, ( vec3 2.2 0 0, ( 0, vec3 1 0 0 ) ) )
            , ( Da, ( vec3 3.3 0 0, ( 0, vec3 1 0 0 ) ) )
            , ( Da, ( vec3 0 1 0, ( 0, vec3 0 1 0 ) ) )
            , ( Oka, ( vec3 1.1 1.1 0, ( 0, vec3 0 1 0 ) ) )
            , ( Da, ( vec3 2.2 1.1 0, ( 0, vec3 0 1 0 ) ) )
            , ( Oka, ( vec3 3.3 1.1 1, ( 0, vec3 0 1 0 ) ) )
            ]
      , selected = Nothing
      }
    , Cmd.none
    )


okadaMesh : Vec3 -> Okada -> Mesh Shader.Vertex
okadaMesh color okada =
    case okada of
        Oka ->
            Block.meshOka color

        Da ->
            Block.meshDa color


limitRadian : Float -> Float
limitRadian r =
    max (min r (pi / 2 * 0.99)) (-pi / 2 * 0.99)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( width, height ) =
            model.size

        ( cr, ca, cb ) =
            model.camera
    in
    case msg of
        Delta _ ->
            ( model
            , Cmd.none
            )

        OnDragBy ( dx, dy ) ->
            ( { model
                | camera = ( cr, ca - dx / 30, limitRadian (cb - dy / 30) )
              }
            , Cmd.none
            )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        ClickMsg ( x, y ) ->
            ( { model | selected = getClickedBlock model ( (x * 2) / toFloat width - 1, 1 - y / toFloat height * 2 ), position = ( x, y ) }, Cmd.none )


getClickedBlock : Model -> ( Float, Float ) -> Maybe BlockWithGeo
getClickedBlock model pos =
    let
        origin =
            Shader.orbitCamelaPosition model.camera

        destination =
            Shader.getClickPosition model.camera (getPerspective model.size) pos

        direction =
            Vec3.direction destination origin
    in
    List.filter
        (\( _, geo ) ->
            let
                ( p, ( rad, axis ) ) =
                    geo

                mat =
                    Mat4.mul (Mat4.makeRotate rad axis) (Mat4.makeTranslate p)

                triangleList =
                    List.map (\( v0, v1, v2 ) -> ( Mat4.transform mat v0, Mat4.transform mat v1, Mat4.transform mat v2 )) (List.concat Asset.cube)

                isClicked =
                    Shader.isCubeClicked origin direction triangleList
            in
            isClicked
        )
        model.blocks
        |> List.head


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta (\dt -> Delta dt)
        , Draggable.subscriptions DragMsg model.drag
        ]


view : Model -> Browser.Document Msg
view model =
    let
        ( w, h ) =
            model.size

        ( x, y ) =
            model.position

        perspective =
            getPerspective model.size
    in
    { title = "岡田集め - リメイク"
    , body =
        [ Html.div []
            [ Html.text
                (let
                    selected =
                        case model.selected of
                            Just _ ->
                                "selected"

                            Nothing ->
                                "none"
                 in
                 selected ++ " | " ++ String.fromFloat x ++ ", " ++ String.fromFloat y
                )
            ]
        , WebGL.toHtml
            ([ width w
             , height h
             , style "display" "block"
             , style "border" "1px solid black"
             , Draggable.mouseTrigger "my-element" DragMsg
             , Mouse.onClick (.offsetPos >> ClickMsg)
             ]
                ++ Draggable.touchTriggers "my-element" DragMsg
            )
            (entities model.camera
                perspective
                (List.filter
                    (\block ->
                        case model.selected of
                            Just b ->
                                block /= b

                            Nothing ->
                                True
                    )
                    model.blocks
                )
                ++ (case model.selected of
                        Just b ->
                            [ entity (vec3 100 0 0) model.camera perspective b ]

                        Nothing ->
                            []
                   )
            )
        ]
    }


entities : Shader.OrbitCamela -> Mat4 -> List ( Okada, Shader.Geo ) -> List WebGL.Entity
entities camera perspective list =
    List.map
        (\( okada, geo ) ->
            entity (vec3 245 121 0) camera perspective ( okada, geo )
        )
        list


entity : Vec3 -> Shader.OrbitCamela -> Mat4 -> ( Okada, Shader.Geo ) -> WebGL.Entity
entity color camera perspective ( okada, geo ) =
    WebGL.entity
        Shader.vertexShader
        Shader.fragmentShader
        (okadaMesh color okada)
        (Shader.uniforms camera perspective geo)


getPerspective : ( Int, Int ) -> Mat4
getPerspective ( width, height ) =
    Mat4.makePerspective 45 (toFloat width / toFloat height) 0.01 100
