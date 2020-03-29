module Block exposing (Vertex, mesh)

import Math.Matrix4 as Mat4 exposing (Mat4)
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


mesh : Geo -> Mesh Vertex
mesh geo =
    let
        rft =
            convert geo (vec3 1 1 1)

        lft =
            convert geo (vec3 -1 1 1)

        lbt =
            convert geo (vec3 -1 -1 1)

        rbt =
            convert geo (vec3 1 -1 1)

        rbb =
            convert geo (vec3 1 -1 -1)

        rfb =
            convert geo (vec3 1 1 -1)

        lfb =
            convert geo (vec3 -1 1 -1)

        lbb =
            convert geo (vec3 -1 -1 -1)
    in
    [ face (vec3 115 210 22) rft rfb rbb rbt -- green
    , face (vec3 52 101 164) rft rfb lfb lft -- blue
    , face (vec3 237 212 0) rft lft lbt rbt -- yellow
    , face (vec3 204 0 0) rfb lfb lbb rbb -- red
    , face (vec3 117 80 123) lft lfb lbb lbt -- purple
    , face (vec3 245 121 0) rbt rbb lbb lbt -- orange
    ]
        |> List.concat
        |> WebGL.triangles


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face color a b c d =
    let
        vertex position =
            Vertex (Vec3.scale (1 / 255) color) position
    in
    [ ( vertex a, vertex b, vertex c )
    , ( vertex c, vertex d, vertex a )
    ]
