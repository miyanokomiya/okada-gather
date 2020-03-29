module Block exposing (Vertex, mesh, meshListDa, meshListOka)

import Array
import Asset
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, add, vec3)
import WebGL exposing (Mesh)


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


type alias Rotation =
    ( Float, Vec3 )


type alias Geo =
    ( Vec3, Rotation )


convert : Geo -> Vec3 -> Vec3
convert ( position, ( radian, axis ) ) vec =
    vec
        |> Mat4.transform (Mat4.rotate radian axis Mat4.identity)
        |> add position


convertTriangle : Geo -> List Triangle -> List Triangle
convertTriangle geo list =
    List.map
        (\( a, b, c ) -> ( convert geo a, convert geo b, convert geo c ))
        list


type alias Triangle =
    ( Vec3, Vec3, Vec3 )


mesh : List Triangle -> Geo -> Mesh Vertex
mesh list geo =
    list
        |> convertTriangle geo
        |> List.map (\tri -> face (vec3 245 121 0) tri)
        |> List.concat
        |> WebGL.triangles


meshListOka : Geo -> List (Mesh Vertex)
meshListOka geo =
    Asset.oka
        |> develop3D
        |> List.map (\triangles -> mesh triangles geo)


meshListDa : Geo -> List (Mesh Vertex)
meshListDa geo =
    Asset.da
        |> develop3D
        |> List.map (\triangles -> mesh triangles geo)


develop3D : List (List Triangle) -> List (List Triangle)
develop3D org =
    List.concat
        [ org
        , bottom org
        ]


side : List (List Triangle) -> List (List Triangle)
side list =
    let
        adjusted =
            case List.head list of
                Just t ->
                    List.append list [ t ]

                Nothing ->
                    list

        arr =
            Array.fromList adjusted
    in
    adjusted
        |> List.indexedMap
            (\i item ->
                let
                    next =
                        Array.get (i + 1) arr
                in
                case next of
                    Just a ->
                        [ item ]

                    Nothing ->
                        [ item ]
            )
        |> List.concat


bottom : List (List Triangle) -> List (List Triangle)
bottom list =
    list
        |> List.map
            (\triangles ->
                List.map
                    (\( a, b, c ) ->
                        ( Vec3.sub a (vec3 0 0 0.1)
                        , Vec3.sub b (vec3 0 0 0.1)
                        , Vec3.sub c (vec3 0 0 0.1)
                        )
                    )
                    triangles
            )


face : Vec3 -> ( Vec3, Vec3, Vec3 ) -> List ( Vertex, Vertex, Vertex )
face color ( a, b, c ) =
    let
        vertex position =
            Vertex (Vec3.scale (1 / 255) color) position
    in
    [ ( vertex a, vertex b, vertex c )
    ]
