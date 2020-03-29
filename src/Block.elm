module Block exposing (Vertex, Geo, mesh, meshListDa, meshListOka)

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
        -- |> Mat4.transform (Mat4.rotate radian axis Mat4.identity)
        -- |> add position


convertTriangle : Geo -> List Triangle -> List Triangle
convertTriangle geo list =
    List.map
        (\( a, b, c ) -> ( convert geo a, convert geo b, convert geo c ))
        list


type alias Triangle =
    ( Vec3, Vec3, Vec3 )


mesh : Geo -> List Triangle -> Mesh Vertex
mesh geo list =
    list
        |> convertTriangle geo
        |> List.map (\tri -> face (vec3 245 121 0) tri)
        |> List.concat
        |> WebGL.triangles


meshListOka : Geo -> Mesh Vertex
meshListOka geo =
    Asset.oka
        |> List.concat
        |> mesh geo


meshListDa : Geo -> Mesh Vertex
meshListDa geo =
    Asset.da
        |> List.concat
        |> mesh geo


face : Vec3 -> ( Vec3, Vec3, Vec3 ) -> List ( Vertex, Vertex, Vertex )
face color ( a, b, c ) =
    let
        vertex position =
            Vertex (Vec3.scale (1 / 255) color) position
    in
    [ ( vertex a, vertex b, vertex c )
    ]
