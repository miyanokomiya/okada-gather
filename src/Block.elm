module Block exposing (mesh, meshDa, meshOka)

import Array
import Asset
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, add, vec3)
import Shader
import WebGL exposing (Mesh)



-- |> Mat4.transform (Mat4.rotate radian axis Mat4.identity)
-- |> add position


mesh : Vec3 -> List Shader.Triangle -> Mesh Shader.Vertex
mesh color list =
    list
        |> List.map (\tri -> face color tri)
        |> List.concat
        |> WebGL.triangles


meshOka : Vec3 -> Mesh Shader.Vertex
meshOka color =
    Asset.oka
        |> List.concat
        |> mesh color


meshDa : Vec3 -> Mesh Shader.Vertex
meshDa color =
    Asset.da
        |> List.concat
        |> mesh color


face : Vec3 -> ( Vec3, Vec3, Vec3 ) -> List ( Shader.Vertex, Shader.Vertex, Shader.Vertex )
face color ( a, b, c ) =
    let
        vertex position =
            Shader.Vertex (Vec3.scale (1 / 255) color) position
    in
    [ ( vertex a, vertex b, vertex c )
    ]
