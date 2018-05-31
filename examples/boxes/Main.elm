module Boxes exposing (main)

import AnimationFrame
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Html.Events exposing (onClick)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics
import Task
import Time exposing (Time)
import WebGL exposing (Entity, Shader, Mesh)
import Window
import Random


type alias Model =
    { screenWidth : Int
    , screenHeight : Int
    , world : Physics.World
    , shapes : ShapeDict
    }


type Msg
    = Tick Time
    | Resize Window.Size
    | AddRandomBox
    | AddBox ( Physics.Body, Physics.ShapeId )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Orientation =
    Physics.Body -> Physics.Body


{-| A cube-shaped body with unit sides, default mass 5,
and custom orientation.
Orienting the body with a function (chain) prior to addShape makes chaining easier
here and in the caller which must also track the ShapeId generated here.
-}
box : Orientation -> ( Physics.Body, Physics.ShapeId )
box orient =
    Physics.body
        |> orient
        |> Physics.setMass 5
        |> Physics.addShape (Physics.box (vec3 1 1 1))


{-| A box raised above the plane and rotated to a random 3d angle
-}
randomlyRotatedBox : Random.Generator ( Physics.Body, Physics.ShapeId )
randomlyRotatedBox =
    Random.map4
        (\angle x y z ->
            box
                (Physics.offsetBy (vec3 0 0 10)
                    >> Physics.rotateBy (Vec3.normalize (vec3 x y z)) angle
                )
        )
        (Random.float (-pi / 2) (pi / 2))
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.float -1 1)


init : ( Model, Cmd Msg )
init =
    let
        initialBodies =
            [ ( Physics.body
                    |> Physics.offsetBy (vec3 0 0 -1)
                    |> Physics.addShape Physics.plane
              , planeMesh
              )
            , ( box
                    (Physics.offsetBy (vec3 0 0 2)
                        >> Physics.rotateBy Vec3.j (-pi / 5)
                    )
              , cubeMesh
              )
            , ( box
                    (Physics.offsetBy (vec3 -1.2 0 9)
                        >> Physics.rotateBy Vec3.j (-pi / 4)
                    )
              , cubeMesh
              )
            , ( box
                    (Physics.offsetBy (vec3 1.3 0 6)
                        >> Physics.rotateBy Vec3.j (pi / 5)
                    )
              , cubeMesh
              )
            ]

        initialWorld =
            Physics.world
                |> Physics.setGravity (vec3 0 0 -10)

        ( world, shapes ) =
            List.foldl
                addBody
                ( initialWorld, Dict.empty )
                initialBodies
    in
        ( { -- replaced by resize, including the initial resize
            screenWidth = 1
          , screenHeight = 1
          -- continuously updated by ticks and clicks
          , world = world
          -- updated by clicks
          , shapes = shapes
          }
        , Task.perform Resize Window.size
        )


addBody : ( ( Physics.Body, Physics.ShapeId ), Mesh Attributes ) -> ( Physics.World, ShapeDict ) -> ( Physics.World, ShapeDict )
addBody ( ( body, shapeId ), mesh ) ( world, shapes ) =
    let
        ( nextWorld, bodyId ) =
            Physics.addBody body world

        nextShapes = insertShape bodyId shapeId mesh shapes
    in
        ( nextWorld, nextShapes )


type alias ShapeDict =
    Dict (Physics.BodyId, Physics.ShapeId) (Mesh Attributes)


insertShape : Physics.BodyId -> Physics.ShapeId -> Mesh Attributes -> ShapeDict -> ShapeDict
insertShape bodyId shapeId mesh shapes =
    Dict.insert ( bodyId, shapeId ) mesh shapes


getShape : Physics.BodyId -> Physics.ShapeId -> ShapeDict -> Mesh Attributes
getShape bodyId shapeId shapes =
    Dict.get (bodyId, shapeId) shapes
        -- This default will never be used as all valid bodyid and shapeid 
        -- pairs map to a mesh.  This example code COULD be optimized to 
        -- RELY on this default, skipping ShapeDict entries for unit cubes,
        -- which is the predominant case.
        |> Maybe.withDefault cubeMesh


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize { width, height } ->
            ( { model
                | screenWidth = width
                , screenHeight = height
              }
            , Cmd.none
            )

        AddRandomBox ->
            ( model, Random.generate AddBox randomlyRotatedBox )

        AddBox generatedBox ->
            ( let
                ( world, shapes ) =
                    addBody ( generatedBox, cubeMesh ) ( model.world, model.shapes )
              in
                { model | world = world, shapes = shapes }
            , Cmd.none
            )

        Tick dt ->
            ( { model | world = Physics.step (1 / 60) model.world }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , AnimationFrame.diffs Tick
        ]



-- View:


view : Model -> Html Msg
view { screenWidth, screenHeight, world, shapes } =
    WebGL.toHtmlWith
        [ WebGL.depth 1
        , WebGL.alpha True
        , WebGL.antialias
        , WebGL.clearColor 0 0 0 1
        ]
        [ width screenWidth
        , height screenHeight
        , style
            [ ( "display", "block" )
            , ( "width", toString screenWidth ++ "px" )
            , ( "height", toString screenHeight ++ "px" )
            ]
        , onClick AddRandomBox
        ]
        (let
            camera =
                Mat4.makeLookAt (Vec3.vec3 0 30 20) (Vec3.vec3 0 0 0) Vec3.k

            aspectRatio =
                (toFloat screenWidth) / (toFloat screenHeight)

            perspective =
                Mat4.makePerspective 24 aspectRatio 5 2000
         in
            Physics.foldl (addShape shapes camera perspective) [] world
                |> (if debugContacts then
                        addContacts camera perspective world
                    else
                        identity
                   )
        )


{-| Set to True to see collision points
-}
debugContacts : Bool
debugContacts =
    False


addShape : ShapeDict -> Mat4 -> Mat4 -> { transform : Mat4, bodyId : Int, shapeId : Int } -> List Entity -> List Entity
addShape shapes camera perspective { transform, bodyId, shapeId } tail =
    WebGL.entity
        vertex
        fragment
        (getShape bodyId shapeId shapes)
        { camera = camera
        , perspective = perspective
        , transform = transform
        }
        :: tail



-- Meshes:


type alias Attributes =
    { position : Vec3
    , color : Vec3
    }


planeMesh : Mesh Attributes
planeMesh =
    WebGL.triangles
        (face
            (vec3 10 10 0)
            (vec3 -10 10 0)
            (vec3 -10 -10 0)
            (vec3 10 -10 0)
            (vec3 0.2 0.2 0.2)
        )


cubeMesh : Mesh Attributes
cubeMesh =
    let
        rft =
            vec3 1 1 1

        lft =
            vec3 -1 1 1

        lbt =
            vec3 -1 -1 1

        rbt =
            vec3 1 -1 1

        rbb =
            vec3 1 -1 -1

        rfb =
            vec3 1 1 -1

        lfb =
            vec3 -1 1 -1

        lbb =
            vec3 -1 -1 -1
    in
        [ face rft rfb rbb rbt (vec3 0.4 0.4 0.4)
        , face rft rfb lfb lft (vec3 0.5 0.5 0.5)
        , face rft lft lbt rbt (vec3 0.6 0.6 0.6)
        , face rfb lfb lbb rbb (vec3 0.7 0.7 0.7)
        , face lft lfb lbb lbt (vec3 0.8 0.8 0.8)
        , face rbt rbb lbb lbt (vec3 0.9 0.9 0.9)
        ]
            |> List.concat
            |> WebGL.triangles


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Attributes, Attributes, Attributes )
face a b c d color =
    [ ( Attributes a color, Attributes b color, Attributes c color )
    , ( Attributes c color, Attributes d color, Attributes a color )
    ]



-- Shaders:


type alias Uniforms =
    { camera : Mat4
    , perspective : Mat4
    , transform : Mat4
    }


type alias Varying =
    { vcolor : Vec3 }


vertex : Shader Attributes Uniforms Varying
vertex =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 camera;
        uniform mat4 perspective;
        uniform mat4 transform;
        varying vec3 vcolor;
        void main () {
          gl_Position = perspective * camera * transform * vec4(position, 1.0);
          vcolor = color;
        }
    |]


fragment : Shader {} Uniforms Varying
fragment =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        void main () {
          gl_FragColor = vec4(vcolor, 1.0);
        }
    |]


{-| Render collision points for the purpose of debugging
-}
addContacts : Mat4 -> Mat4 -> Physics.World -> List Entity -> List Entity
addContacts camera perspective world entities =
    let
        addContact : Vec3 -> List Entity -> List Entity
        addContact contactPoint tail =
            WebGL.entity
                vertex
                fragment
                cubeMesh
                { camera = camera
                , perspective = perspective
                , transform =
                    Mat4.mul
                        (Mat4.makeTranslate contactPoint)
                        (Mat4.makeScale (vec3 0.1 0.1 0.1))
                }
                :: tail
    in
        List.foldl
            addContact
            entities
            (Physics.contacts world)
