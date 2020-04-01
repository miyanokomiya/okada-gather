module Block exposing (mesh, meshDa, meshOka)

import Array
import Asset
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, add, vec3)
import Shader
import WebGL exposing (Mesh)



-- |> Mat4.transform (Mat4.rotate radian axis Mat4.identity)
-- |> add position


mesh : List ( Shader.Triangle, Vec3 ) -> Mesh Shader.Vertex
mesh list =
    list
        |> List.map (\( tri, color ) -> face color tri)
        |> List.concat
        |> WebGL.triangles


meshOka : Vec3 -> Vec3 -> Mesh Shader.Vertex
meshOka faceColor sideColor =
    List.append
        (Asset.okaFace
            |> List.concat
            |> List.map (\tri -> ( tri, faceColor ))
        )
        (Asset.okaSide
            |> List.concat
            |> List.map (\tri -> ( tri, sideColor ))
        )
        |> mesh


meshDa : Vec3 -> Vec3 -> Mesh Shader.Vertex
meshDa faceColor sideColor =
    List.append
        (Asset.daFace
            |> List.concat
            |> List.map (\tri -> ( tri, faceColor ))
        )
        (Asset.daSide
            |> List.concat
            |> List.map (\tri -> ( tri, sideColor ))
        )
        |> mesh


face : Vec3 -> ( Vec3, Vec3, Vec3 ) -> List ( Shader.Vertex, Shader.Vertex, Shader.Vertex )
face color ( a, b, c ) =
    let
        vertex position =
            Shader.Vertex (Vec3.scale (1 / 255) color) position
    in
    [ ( vertex a, vertex b, vertex c )
    ]
