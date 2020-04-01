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


type Status
    = Default
    | Selected
    | Completed


type alias GeoBlock =
    { id : Int
    , okada : Okada
    , geo : Shader.Geo
    , status : Status
    }


type alias Model =
    { size : ( Int, Int )
    , camera : Shader.OrbitCamela
    , downTime : Float
    , drag : Draggable.State String
    , blocks : List GeoBlock
    , selected : Maybe GeoBlock
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
      , camera = ( 14, 0, 0 )
      , downTime = 0
      , drag = Draggable.init
      , blocks =
            [ { id = 1, okada = Oka, geo = ( vec3 0 0 0, Mat4.makeRotate 0 (vec3 1 0 0) ), status = Default }
            , { id = 2, okada = Da, geo = ( vec3 1.1 0 0, Mat4.makeRotate 0 (vec3 1 0 0) ), status = Default }
            , { id = 3, okada = Da, geo = ( vec3 0 1.1 0, Mat4.makeRotate (pi / 2) (vec3 0 1 0) ), status = Default }
            , { id = 4, okada = Oka, geo = ( vec3 1.1 1.1 0, Mat4.makeRotate (pi / 2) (vec3 0 1 0) ), status = Default }
            ]
      , selected = Nothing
      }
    , Cmd.none
    )


okadaMesh : Vec3 -> Vec3 -> Okada -> Mesh Shader.Vertex
okadaMesh faceColor sideColor okada =
    case okada of
        Oka ->
            Block.meshOka faceColor sideColor

        Da ->
            Block.meshDa faceColor sideColor


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
                , downTime = model.downTime + 1
              }
            , Cmd.none
            )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        ClickMsg ( x, y ) ->
            if model.downTime < 10 then
                ( { model
                    | selected = getClickedBlock model ( (x * 2) / toFloat width - 1, 1 - y / toFloat height * 2 )
                    , downTime = 0
                  }
                , Cmd.none
                )

            else
                ( { model | downTime = 0 }, Cmd.none )


getClickedBlock : Model -> ( Float, Float ) -> Maybe GeoBlock
getClickedBlock model pos =
    let
        origin =
            Shader.orbitCamelaPosition model.camera

        destination =
            Shader.getClickPosition model.camera (getPerspective model.size) pos

        direction =
            Vec3.direction destination origin
    in
    model.blocks
        |> List.map
            (\block ->
                let
                    ( p, r ) =
                        block.geo

                    mat =
                        Mat4.mul (Mat4.makeTranslate p) r

                    triangles =
                        List.map (\( v0, v1, v2 ) -> ( Mat4.transform mat v0, Mat4.transform mat v1, Mat4.transform mat v2 )) (List.concat Asset.cube)
                in
                ( triangles, block )
            )
        |> Shader.nearestClickedMesh origin direction


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
                 selected ++ " | " ++ String.fromFloat model.downTime
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
                            [ entity (vec3 80 0 0) (vec3 100 0 0) model.camera perspective b ]

                        Nothing ->
                            []
                   )
            )
        ]
    }


entities : Shader.OrbitCamela -> Mat4 -> List GeoBlock -> List WebGL.Entity
entities camera perspective list =
    List.map
        (\block ->
            entity (vec3 145 121 0) (vec3 245 121 0) camera perspective block
        )
        list


entity : Vec3 -> Vec3 -> Shader.OrbitCamela -> Mat4 -> GeoBlock -> WebGL.Entity
entity faceColor sideColor camera perspective block =
    WebGL.entity
        Shader.vertexShader
        Shader.fragmentShader
        (okadaMesh faceColor sideColor block.okada)
        (Shader.uniforms camera perspective block.geo)


getPerspective : ( Int, Int ) -> Mat4
getPerspective ( width, height ) =
    Mat4.makePerspective 45 (toFloat width / toFloat height) 0.01 100
