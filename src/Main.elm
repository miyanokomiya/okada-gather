module Main exposing (main)

import Asset
import Block
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Draggable
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Shader
import WebGL exposing (Mesh)


type Okada
    = Oka
    | Da


type alias GeoBlock =
    { id : Int
    , okada : Okada
    , geo : Shader.Geo
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


type alias MeshSet =
    { oka : Mesh Shader.Vertex
    , da : Mesh Shader.Vertex
    }


type alias Model =
    { size : ( Int, Int )
    , level : Int
    , camera : Shader.OrbitCamela
    , downTime : Float
    , drag : Draggable.State String
    , blocks : List GeoBlock
    , pairs : List OkadaPair
    , selected : Maybe GeoBlock
    , meshMap :
        { default : MeshSet
        , selected : MeshSet
        , completed : MeshSet
        }
    }


type Msg
    = Reset
    | Next
    | Delta Float
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
    ( initModel 1, Cmd.none )


countAtLevel : Int -> Int
countAtLevel level =
    1 * (level * 2 - 1)


initModel : Int -> Model
initModel level =
    { size = ( 400, 600 )
    , level = level
    , camera = ( 4 + toFloat (4 * level), pi / 8, pi / 16 )
    , downTime = 0
    , drag = Draggable.init
    , blocks =
        let
            count =
                toFloat (countAtLevel level)
        in
        List.range 0 (round count)
            |> List.map
                (\i ->
                    List.range 0 (round count)
                        |> List.map
                            (\j ->
                                let
                                    okada =
                                        if modBy 2 (i + j) == 0 then
                                            Oka

                                        else
                                            Da
                                in
                                { id = i * 1000 + j
                                , okada = okada
                                , geo = ( vec3 (1.1 * (toFloat j - (count / 2))) (1.1 * (toFloat i - (count / 2))) 0, Mat4.makeRotate 0 (vec3 1 0 0) )
                                }
                            )
                )
            |> List.concat
    , pairs = []
    , selected = Nothing
    , meshMap =
        { default = { oka = okadaMesh defaultColor Oka, da = okadaMesh defaultColor Da }
        , selected = { oka = okadaMesh selectedColor Oka, da = okadaMesh selectedColor Da }
        , completed = { oka = okadaMesh completedColor Oka, da = okadaMesh completedColor Da }
        }
    }


nextLevel : Model -> Model
nextLevel model =
    initModel (model.level + 1)


okadaMesh : ColorPair -> Okada -> Mesh Shader.Vertex
okadaMesh ( faceColor, sideColor ) okada =
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
        Reset ->
            init ()

        Next ->
            ( nextLevel model, Cmd.none )

        Delta dt ->
            if isLevelCompleted model && model.downTime == 0 then
                let
                    ( a, b, c ) =
                        model.camera
                in
                ( { model | camera = ( a, b + (dt / 1000), c ) }
                , Cmd.none
                )

            else
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
            { model
                | selected = Nothing
                , blocks =
                    model.blocks
                        ++ (case model.selected of
                                Just b ->
                                    [ b ]

                                Nothing ->
                                    []
                           )
            }

        Just block ->
            case model.selected of
                Nothing ->
                    { model
                        | selected = maybeBlock
                        , blocks = List.filter (\b -> b /= block) model.blocks
                    }

                Just current ->
                    if block == current then
                        { model
                            | selected = Nothing
                            , blocks = model.blocks ++ [ current ]
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
                                    , blocks = List.filter (\b -> b /= block) model.blocks
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
    (model.blocks ++ Maybe.withDefault [] (Maybe.map (\b -> [ b ]) model.selected))
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


isLevelCompleted : Model -> Bool
isLevelCompleted model =
    List.length model.blocks == 0


isAllLevelCompleted : Model -> Bool
isAllLevelCompleted model =
    model.level == 1 && isLevelCompleted model


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
        [ Html.div
            [ style "display" "flex"
            , style "justify-content" "center"
            ]
            [ Html.div
                [ width w
                ]
                [ Html.div
                    []
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
                    (entities
                        model.camera
                        perspective
                        model.meshMap.default
                        model.blocks
                        ++ Maybe.withDefault [] (Maybe.map (\b -> [ entity model.camera perspective model.meshMap.selected b ]) model.selected)
                        ++ entities
                            model.camera
                            perspective
                            model.meshMap.completed
                            (model.pairs
                                |> List.map
                                    (\( oka, da ) -> [ oka, da ])
                                |> List.concat
                            )
                    )
                , Html.div
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "justify-content" "space-between"
                    , Html.Attributes.style "margin-top" "0.2rem"
                    ]
                    [ Html.div []
                        [ button
                            [ Html.Events.onClick Reset
                            ]
                            [ Html.text "RESET" ]
                        ]
                    , Html.div
                        [ Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "align-items" "center"
                        ]
                        ((if isLevelCompleted model then
                            if isAllLevelCompleted model then
                                [ button
                                    [ width 100
                                    , Html.Events.onClick Next
                                    , Html.Attributes.style "margin" "0 1rem"
                                    , Html.Attributes.style "background-color" "gold"
                                    ]
                                    [ Html.text "YOU MUST BE OKADA!!!" ]
                                ]

                            else
                                [ button
                                    [ width 100
                                    , Html.Events.onClick Next
                                    , Html.Attributes.style "margin" "0 1rem"
                                    , Html.Attributes.style "background-color" "blue"
                                    ]
                                    [ Html.text "NEXT" ]
                                ]

                          else
                            []
                         )
                            ++ [ Html.span
                                    [ Html.Attributes.style "margin" "0 1rem"
                                    ]
                                    [ Html.text ("Lv. " ++ String.fromInt model.level) ]
                               , Html.span
                                    [ Html.Attributes.style "min-width" "40px"
                                    , Html.Attributes.style "text-align" "right"
                                    ]
                                    [ Html.text (String.fromInt (List.length model.pairs * 2)) ]
                               , Html.span
                                    [ Html.Attributes.style "margin" "0 0.4rem"
                                    ]
                                    [ Html.text "/" ]
                               , Html.span [] [ Html.text (String.fromInt (countAtLevel model.level * countAtLevel model.level)) ]
                               ]
                        )
                    ]
                ]
            ]
        ]
    }


button : List (Html.Attribute msg) -> List (Html msg) -> Html msg
button attrs children =
    Html.button
        ([ Html.Attributes.style "padding" "0.4rem 0.8rem"
         , Html.Attributes.style "background-color" "#444"
         , Html.Attributes.style "color" "white"
         , Html.Attributes.style "border-radius" "4px"
         , Html.Attributes.style "border" "none"
         ]
            ++ attrs
        )
        children


entities : Shader.OrbitCamela -> Mat4 -> MeshSet -> List GeoBlock -> List WebGL.Entity
entities camera perspective set list =
    List.map
        (\block ->
            entity camera perspective set block
        )
        list


entity : Shader.OrbitCamela -> Mat4 -> MeshSet -> GeoBlock -> WebGL.Entity
entity camera perspective set block =
    WebGL.entity
        Shader.vertexShader
        Shader.fragmentShader
        (toMesh set block)
        (Shader.uniforms camera perspective block.geo)


toMesh : MeshSet -> GeoBlock -> Mesh Shader.Vertex
toMesh set block =
    if block.okada == Oka then
        set.oka

    else
        set.da


getPerspective : ( Int, Int ) -> Mat4
getPerspective ( width, height ) =
    Mat4.makePerspective 45 (toFloat width / toFloat height) 0.01 100
