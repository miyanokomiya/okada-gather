module Block exposing (Geo, Vertex, mesh, meshDa, meshOka)

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


type alias Triangle =
    ( Vec3, Vec3, Vec3 )


mesh : List Triangle -> Mesh Vertex
mesh list =
    list
        |> List.map (\tri -> face (vec3 245 121 0) tri)
        |> List.concat
        |> WebGL.triangles


meshOka : Mesh Vertex
meshOka =
    Asset.oka
        |> List.concat
        |> mesh


meshDa : Mesh Vertex
meshDa =
    Asset.da
        |> List.concat
        |> mesh


face : Vec3 -> ( Vec3, Vec3, Vec3 ) -> List ( Vertex, Vertex, Vertex )
face color ( a, b, c ) =
    let
        vertex position =
            Vertex (Vec3.scale (1 / 255) color) position
    in
    [ ( vertex a, vertex b, vertex c )
    ]
