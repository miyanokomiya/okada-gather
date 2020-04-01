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


type alias OkadaPair =
    ( GeoBlock, GeoBlock )


type alias ColorPair =
    ( Vec3, Vec3 )


defaultColor : ColorPair
defaultColor =
    ( vec3 60 179 113, vec3 47 79 79 )


selectedColor : ColorPair
selectedColor =
    ( vec3 165 42 42, vec3 204 0 153 )


completedColor : ColorPair
completedColor =
    ( vec3 255 215 0, vec3 255 165 0 )


type alias Model =
    { size : ( Int, Int )
    , camera : Shader.OrbitCamela
    , downTime : Float
    , drag : Draggable.State String
    , blocks : List GeoBlock
    , pairs : List OkadaPair
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
            let
                count =
                  1
            in
            List.range 0 count
                |> List.map
                    (\i ->
                        List.range 0 count
                            |> List.map
                                (\j ->
                                    { id = i * 1000 + j
                                    , okada =
                                        if modBy 2 (i + j) == 0 then
                                            Oka

                                        else
                                            Da
                                    , geo = ( vec3 (1.1 * (toFloat j - (count / 2))) (1.1 * (toFloat i - (count / 2))) 0, Mat4.makeRotate 0 (vec3 1 0 0) )
                                    , status = Default
                                    }
                                )
                    )
                |> List.concat
      , pairs = []
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
                let
                    nextModel =
                        clickBlock model (getClickedBlock model ( (x * 2) / toFloat width - 1, 1 - y / toFloat height * 2 ))
                in
                ( { nextModel | downTime = 0 }
                , Cmd.none
                )

            else
                ( { model | downTime = 0 }, Cmd.none )


clickBlock : Model -> Maybe GeoBlock -> Model
clickBlock model maybeBlock =
    case maybeBlock of
        Nothing ->
            { model | selected = Nothing }

        Just block ->
            case model.selected of
                Nothing ->
                    { model | selected = maybeBlock }

                Just current ->
                    if block == current then
                        { model
                            | selected = Nothing
                        }

                    else
                        let
                            pair =
                                createPair block current
                        in
                        case pair of
                            Just p ->
                                { model
                                    | selected = Nothing
                                    , blocks = List.filter (\b -> b /= block && b /= current) model.blocks
                                    , pairs = List.append model.pairs [ p ]
                                }

                            Nothing ->
                                model


createPair : GeoBlock -> GeoBlock -> Maybe OkadaPair
createPair a b =
    if a.okada == Oka && b.okada == Da then
        Just ( a, b )

    else if a.okada == Da && b.okada == Oka then
        Just ( b, a )

    else
        Nothing


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
            (entities defaultColor
                model.camera
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
                            [ entity selectedColor model.camera perspective b ]

                        Nothing ->
                            []
                   )
                ++ entities completedColor
                    model.camera
                    perspective
                    (model.pairs
                        |> List.map
                            (\( oka, da ) -> [ oka, da ])
                        |> List.concat
                    )
            )
        ]
    }


entities : ColorPair -> Shader.OrbitCamela -> Mat4 -> List GeoBlock -> List WebGL.Entity
entities colorPair camera perspective list =
    List.map
        (\block ->
            entity colorPair camera perspective block
        )
        list


entity : ColorPair -> Shader.OrbitCamela -> Mat4 -> GeoBlock -> WebGL.Entity
entity ( faceColor, sideColor ) camera perspective block =
    WebGL.entity
        Shader.vertexShader
        Shader.fragmentShader
        (okadaMesh faceColor sideColor block.okada)
        (Shader.uniforms camera perspective block.geo)


getPerspective : ( Int, Int ) -> Mat4
getPerspective ( width, height ) =
    Mat4.makePerspective 45 (toFloat width / toFloat height) 0.01 100
