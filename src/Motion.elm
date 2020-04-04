module Motion exposing
    ( GeoAnimation
    , PositionAnimation
    , animateGeo
    , animatePosition
    , animateRotate
    , positionAnimation
    , repeatAnimation
    , rotateAnimation
    , staticPositionAnimation
    )

import Animation exposing (Animation)
import Block
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Shader


repeatAnimation : Animation.Clock -> Animation -> Animation
repeatAnimation clock animation =
    if Animation.isDone clock animation then
        animation
            |> Animation.delay (Animation.getDuration animation + Animation.getDelay animation)

    else
        animation


rotateAnimation : Float -> Float -> Animation
rotateAnimation clock current =
    Animation.animation clock
        |> Animation.from current
        |> Animation.to (current + pi * 2)
        |> Animation.duration 6000
        |> Animation.delay 0
        |> Animation.ease (\x -> x)


animateRotate : Float -> Animation -> Shader.Geo -> Shader.Geo
animateRotate t animation geo =
    let
        ( pos, rotation ) =
            geo

        next =
            Animation.animate t animation
    in
    ( pos, Mat4.mul rotation (Mat4.makeRotate 0.01 (vec3 0 1 0)) )


type alias PositionAnimation =
    { x : Animation
    , y : Animation
    , z : Animation
    }


type alias GeoAnimation =
    { position : PositionAnimation
    , rotation : Animation
    }


positionAnimation : Animation.Clock -> Float -> Vec3 -> Vec3 -> PositionAnimation
positionAnimation clock duration from to =
    let
        anim =
            Animation.animation clock
                |> Animation.duration duration
                |> Animation.delay 0
                |> Animation.ease (\x -> x)
    in
    { x = anim |> Animation.from (Vec3.getX from) |> Animation.to (Vec3.getX to)
    , y = anim |> Animation.from (Vec3.getY from) |> Animation.to (Vec3.getY to)
    , z = anim |> Animation.from (Vec3.getZ from) |> Animation.to (Vec3.getZ to)
    }


staticPositionAnimation : Vec3 -> PositionAnimation
staticPositionAnimation vec =
    { x = Animation.static (Vec3.getX vec)
    , y = Animation.static (Vec3.getY vec)
    , z = Animation.static (Vec3.getZ vec)
    }


animatePosition : Float -> PositionAnimation -> Shader.Geo -> Shader.Geo
animatePosition t animation geo =
    let
        ( _, rotation ) =
            geo

        next =
            vec3 (Animation.animate t animation.x) (Animation.animate t animation.y) (Animation.animate t animation.z)
    in
    ( next, rotation )


animateGeo : Float -> GeoAnimation -> Shader.Geo -> Shader.Geo
animateGeo t anim current =
    current
        |> animateRotate t anim.rotation
        |> animatePosition t anim.position
