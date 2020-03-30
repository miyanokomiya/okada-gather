module Block exposing (mesh, meshDa, meshOka)

import Array
import Asset
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, add, vec3)
import Shader
import WebGL exposing (Mesh)


convert : Shader.Geo -> Vec3 -> Vec3
convert ( position, ( radian, axis ) ) vec =
    vec



-- |> Mat4.transform (Mat4.rotate radian axis Mat4.identity)
-- |> add position


type alias Triangle =
    ( Vec3, Vec3, Vec3 )


mesh : List Triangle -> Mesh Shader.Vertex
mesh list =
    list
        |> List.map (\tri -> face (vec3 245 121 0) tri)
        |> List.concat
        |> WebGL.triangles


meshOka : Mesh Shader.Vertex
meshOka =
    Asset.oka
        |> List.concat
        |> mesh


meshDa : Mesh Shader.Vertex
meshDa =
    Asset.da
        |> List.concat
        |> mesh


face : Vec3 -> ( Vec3, Vec3, Vec3 ) -> List ( Shader.Vertex, Shader.Vertex, Shader.Vertex )
face color ( a, b, c ) =
    let
        vertex position =
            Shader.Vertex (Vec3.scale (1 / 255) color) position
    in
    [ ( vertex a, vertex b, vertex c )
    ]
