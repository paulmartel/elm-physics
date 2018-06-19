module ConvexPolyhedron exposing (..)

import Physics.Const as Const
import Physics.ConvexPolyhedron as ConvexPolyhedron exposing (ConvexPolyhedron)
import Expect exposing (Expectation)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Physics.Quaternion as Quaternion
import Physics.Transform as Transform
import Test exposing (..)
import Array.Hamt as Array exposing (Array)


clipFaceAgainstPlane : Test
clipFaceAgainstPlane =
    describe "ConvexPolyhedron.clipFaceAgainstPlane"
        [ test "should return 4 results" <|
            -- Four points 1 unit below the plane z=0
            -- we assume to get back 4
            \_ ->
                ConvexPolyhedron.clipFaceAgainstPlane
                    (vec3 0 0 1)
                    0
                    [ vec3 -0.2 -0.2 -1
                    , vec3 -0.2 0.2 -1
                    , vec3 0.2 0.2 -1
                    , vec3 0.2 -0.2 -1
                    ]
                    |> Expect.equal
                        [ vec3 -0.2 -0.2 -1
                        , vec3 -0.2 0.2 -1
                        , vec3 0.2 0.2 -1
                        , vec3 0.2 -0.2 -1
                        ]
        , test "should return no results" <|
            -- Lower the plane to z=-2
            -- we assume no points back
            \_ ->
                ConvexPolyhedron.clipFaceAgainstPlane
                    (vec3 0 0 1)
                    2
                    [ vec3 -0.2 -0.2 -1
                    , vec3 -0.2 0.2 -1
                    , vec3 0.2 0.2 -1
                    , vec3 0.2 -0.2 -1
                    ]
                    |> Expect.equal []
        , test "should return 4 results for two points below, two over" <|
            -- two points below, two over. We get four points back,
            -- though 2 of them are clipped to the back of the plane
            \_ ->
                ConvexPolyhedron.clipFaceAgainstPlane
                    (vec3 0 0 1)
                    0
                    [ vec3 -2 -2 1
                    , vec3 -2 2 1
                    , vec3 2 2 -1
                    , vec3 2 -2 -1
                    ]
                    |> Expect.equal
                        [ vec3 0 -2 0
                        , vec3 0 2 0
                        , vec3 2 2 -1
                        , vec3 2 -2 -1
                        ]
        ]


clipFaceAgainstHull : Test
clipFaceAgainstHull =
    describe "ConvexPolyhedron.clipFaceAgainstHull"
        [ test "should return 4 results" <|
            \_ ->
                let
                    sepNormal =
                        vec3 0 0 1

                    -- Move the box 0.45 units up
                    -- only 0.05 units of the box will be below plane z=0
                    transform =
                        { position = vec3 0 0 0.45
                        , quaternion = Quaternion.identity
                        }

                    -- points in the plane z
                    worldVertsB =
                        [ vec3 -1.0 -1.0 0
                        , vec3 -1.0 1.0 0
                        , vec3 1.0 1.0 0
                        , vec3 1.0 -1.0 0
                        ]

                    -- We will now clip a face in hullA that is closest to the sepNormal
                    -- against the points in worldVertsB.
                    -- We can expect to get back the 4 corners of the box hullA penetrated 0.05 units
                    -- into the plane worldVertsB we constructed
                in
                    {-
                       [ { point: Vec3 { x: 0.5, y: -0.5, z: 0 },
                             normal: Vec3 { x: 0, y: 0, z: -1 },
                             depth: -0.04999999999999999 },
                         { point: Vec3 { x: -0.5, y: -0.5, z: 0 },
                             normal: Vec3 { x: 0, y: 0, z: -1 },
                             depth: -0.04999999999999999 },
                         { point: Vec3 { x: -0.5, y: 0.5, z: 0 },
                             normal: Vec3 { x: 0, y: 0, z: -1 },
                             depth: -0.04999999999999999 },
                         { point: Vec3 { x: 0.5, y: 0.5, z: 0 },
                             normal: Vec3 { x: 0, y: 0, z: -1 },
                             depth: -0.04999999999999999 } ]
                    -}
                    ConvexPolyhedron.clipFaceAgainstHull
                        transform
                        (boxHull 0.5)
                        sepNormal
                        worldVertsB
                        -100
                        100
                        |> List.length
                        |> Expect.equal 4
        ]


clipAgainstHull : Test
clipAgainstHull =
    describe "ConvexPolyhedron.clipAgainstHull"
        [ test "should return 4 results" <|
            \_ ->
                let
                    hull1 =
                        boxHull 1

                    hull2 =
                        boxHull 1

                    t1 =
                        { position = vec3 0 0 2.1 -- going slightly into another box
                        , quaternion = Quaternion.fromAngleAxis (pi / 2) (vec3 0 1 0)
                        }

                    t2 =
                        { position = vec3 0 0 4
                        , quaternion = Quaternion.fromAngleAxis (pi / 2) (vec3 0 1 0)
                        }
                in
                    {-
                       [ { point: Vec3 { x: 0.9999999999999997, y: 1, z: 3.0000000000000004 },
                           normal: Vec3 { x: -2.220446049250313e-16, y: 0, z: 1 },
                           depth: -0.09999999999999964 },
                         { point: Vec3 { x: 0.9999999999999998, y: -1, z: 3.0000000000000004 },
                           normal: Vec3 { x: -2.220446049250313e-16, y: 0, z: 1 },
                           depth: -0.09999999999999964 },
                         { point: Vec3 { x: -0.9999999999999997, y: -1, z: 3 },
                           normal: Vec3 { x: -2.220446049250313e-16, y: 0, z: 1 },
                           depth: -0.10000000000000009 },
                         { point: Vec3 { x: -0.9999999999999997, y: 1, z: 3 },
                           normal: Vec3 { x: -2.220446049250313e-16, y: 0, z: 1 },
                           depth: -0.10000000000000009 } ]
                    -}
                    case ConvexPolyhedron.findSeparatingAxis t1 hull1 t2 hull2 of
                        Just separatingAxis ->
                            ConvexPolyhedron.clipAgainstHull t1 hull1 t2 hull2 separatingAxis -100 100
                                |> List.length
                                |> Expect.equal 4

                        Nothing ->
                            Expect.fail "Couldn't find separate axis"
        , test "should return 2 results" <|
            \_ ->
                let
                    hull1 =
                        boxHull 0.6

                    hull2 =
                        boxHull 0.5

                    t1 =
                        { position = vec3 -0.5 0 0
                        , quaternion = Quaternion.fromAngleAxis (pi / 2) (vec3 0 0 1)
                        }

                    t2 =
                        { position = vec3 0.5 0 0
                        , quaternion = Quaternion.fromAngleAxis (pi / 4) (vec3 0 0 1)
                        }
                in
                    {-
                       [ { point: Vec3 { x: -0.20710678118654746, y: -5.551115123125783e-17, z: -0.5 },
                           normal: Vec3 { x: 0.9999999999999999, y: 0, z: 0 },
                           depth: -0.30710678118654733 },
                         { point: Vec3 { x: -0.20710678118654746, y: -5.551115123125783e-17, z: 0.5 },
                           normal: Vec3 { x: 0.9999999999999999, y: 0, z: 0 },
                           depth: -0.30710678118654733 } ]
                    -}
                    case ConvexPolyhedron.findSeparatingAxis t1 hull1 t2 hull2 of
                        Just separatingAxis ->
                            ConvexPolyhedron.clipAgainstHull t1 hull1 t2 hull2 separatingAxis -100 100
                                |> List.length
                                |> Expect.equal 2

                        Nothing ->
                            Expect.fail "Couldn't find separate axis"
        , test "should work for the case from the debugger" <|
            \_ ->
                let
                    hull =
                        boxHull 1

                    t1 =
                        { position = vec3 -2.9496035986031215 -0.059705884468658266 0.05803282809897854
                        , quaternion = vec4 -0.022809298766761247 0.006783793446053796 0.002763745916207627 0.9997129976872166
                        }

                    t2 =
                        { position = vec3 -1.7732501140437167 -0.23893989356833145 1.9746722038817583
                        , quaternion = vec4 -0.14987379072976215 0.5294480629310288 0.19937553795533458 -0.8108464653532712
                        }

                    maybeSeparatingAxis =
                        ConvexPolyhedron.findSeparatingAxis t1 hull t2 hull
                in
                    case maybeSeparatingAxis of
                        Just separatingAxis ->
                            ConvexPolyhedron.clipAgainstHull t1 hull t2 hull separatingAxis -100 100
                                |> List.length
                                |> Expect.equal 1

                        {-
                           [ { point: Vec3 { x: -1.9395931897893413, y: -0.620034911301545, z: 0.567836561523491 },
                               normal: Vec3 { x: 0.013437614750654274, y: 0.04564300225339029, z: 0.9988674320724998 },
                               depth: -0.502776622199867 } ]
                        -}
                        Nothing ->
                            Expect.fail "Couldn't find separate axis"
        ]


testSepAxis : Test
testSepAxis =
    describe "ConvexPolyhedron.testSepAxis"
        [ test "returns Just depth" <|
            \_ ->
                Expect.equal
                    (ConvexPolyhedron.testSepAxis
                        { position = vec3 -0.2 0 0
                        , quaternion = Quaternion.identity
                        }
                        (boxHull 0.5)
                        { position = vec3 0.2 0 0
                        , quaternion = Quaternion.identity
                        }
                        (boxHull 0.5)
                        (vec3 1 0 0)
                    )
                    (Just 0.6)
        , test "returns Nothing" <|
            \_ ->
                Expect.equal
                    (ConvexPolyhedron.testSepAxis
                        { position = vec3 -5.2 0 0
                        , quaternion = Quaternion.identity
                        }
                        (boxHull 0.5)
                        { position = vec3 0.2 0 0
                        , quaternion = Quaternion.identity
                        }
                        (boxHull 0.5)
                        (vec3 1 0 0)
                    )
                    Nothing
        , test "works with rotation" <|
            \_ ->
                case
                    (ConvexPolyhedron.testSepAxis
                        { position = vec3 1 0 0
                        , quaternion = Quaternion.identity
                        }
                        (boxHull 0.5)
                        { position = vec3 0.2 0 0
                        , quaternion = Quaternion.fromAngleAxis (pi / 4) (vec3 0 0 1)
                        }
                        (boxHull 0.5)
                        (vec3 1 0 0)
                    )
                of
                    Nothing ->
                        Expect.fail "expected depth"

                    Just value ->
                        Expect.within (Expect.Absolute 0.00001) 0.4071067 value
        ]


findSeparatingAxis : Test
findSeparatingAxis =
    describe "ConvexPolyhedron.findSeparatingAxis"
        [ test "works for offset" <|
            \_ ->
                Expect.equal
                    (ConvexPolyhedron.findSeparatingAxis
                        { position = vec3 -0.2 0 0
                        , quaternion = Quaternion.identity
                        }
                        (boxHull 0.5)
                        { position = vec3 0.2 0 0
                        , quaternion = Quaternion.identity
                        }
                        (boxHull 0.5)
                    )
                    (Just (vec3 -1 0 0))
        , test "works for rotation" <|
            \_ ->
                Expect.equal
                    (ConvexPolyhedron.findSeparatingAxis
                        { position = vec3 -0.2 0 0
                        , quaternion = Quaternion.identity
                        }
                        (boxHull 0.5)
                        { position = vec3 0.2 0 0
                        , quaternion = Quaternion.fromAngleAxis (pi / 4) (vec3 0 0 1)
                        }
                        (boxHull 0.5)
                    )
                    (Just (vec3 -1 0 0))
        ]


project : Test
project =
    describe "ConvexPolyhedron.project"
        [ test "works for the positive x axis" <|
            \_ ->
                Expect.equal
                    (ConvexPolyhedron.project
                        Transform.identity
                        (boxHull 0.5)
                        (vec3 1 0 0)
                    )
                    ( 0.5, -0.5 )
        , test "works for the negative x axis" <|
            \_ ->
                Expect.equal
                    (ConvexPolyhedron.project
                        Transform.identity
                        (boxHull 0.5)
                        (vec3 -1 0 0)
                    )
                    ( 0.5, -0.5 )
        , test "works for the positive y axis" <|
            \_ ->
                Expect.equal
                    (ConvexPolyhedron.project
                        Transform.identity
                        (boxHull 0.5)
                        (vec3 0 1 0)
                    )
                    ( 0.5, -0.5 )
        , test "works for the offset" <|
            \_ ->
                Expect.equal
                    (ConvexPolyhedron.project
                        { quaternion = Quaternion.identity
                        , position = vec3 0 1 0
                        }
                        (boxHull 0.5)
                        (vec3 0 1 0)
                    )
                    ( 1.5, 0.5 )
        , test "works for the rotation and offset" <|
            \_ ->
                (ConvexPolyhedron.project
                    { quaternion = Quaternion.fromAngleAxis (pi / 2) (vec3 1 0 0)
                    , position = vec3 0 1 0
                    }
                    (boxHull 0.5)
                    (vec3 0 1 0)
                )
                    |> Expect.all
                        [ Tuple.first >> Expect.within (Expect.Absolute 0.00001) 1.5
                        , Tuple.second >> Expect.within (Expect.Absolute 0.00001) 0.5
                        ]
        ]


faceNormals : Test
faceNormals =
    describe "ConvexPolyhedron.faceNormals"
        [ test "works for the box" <|
            \_ ->
                boxHull 1
                    |> .normals
                    |> Array.toList
                    |> Expect.equal
                        [ vec3 0 0 -1
                        , vec3 0 0 1
                        , vec3 0 -1 0
                        , vec3 0 1 0
                        , vec3 -1 0 0
                        , vec3 1 0 0
                        ]
        ]


addFaceEdges : Test
addFaceEdges =
    describe "ConvexPolyhedron.addFaceEdges"
        -- Testing addFaceEdges avoid over-testing for exact results from
        -- ConvexPolyhedron.uniqueEdges.
        -- There are several valid representations of the same convex
        -- polyhedron, differing in the listed order of vertices and/or faces
        -- or in insignificant rounding errors in vertex values.
        -- So, the implementation of uniqueEdges should be given some lattitude
        -- in its resulting list of edges. ConvexPolyhedron.addFaceEdges does
        -- most of the work of ConvexPolyhedron.uniqueEdges, and it can be
        -- tested with seed values to get more deterministic results from
        -- ConvexPolyhedrons even with varying equivalent representations.
        [ test "gives the correct number of edges for a box" <|
            \_ ->
                boxHull 1
                    |> uniqueEdgesOfConvexPolyhedron
                    |> List.length
                    |> Expect.equal 3
        , test "works for the box with positive seeds" <|
            \_ ->
                let
                    -- Pre-calculated seeds are one way to get an exact
                    -- normalized result. Members of the seed set are acceptable
                    -- members of the result set. So long as the result-building
                    -- process is non-destructive, the seeds should act as magnets
                    -- for other valid results and should mask them in the final
                    -- result.
                    fullSeedSet =
                        [ vec3 1 0 0
                        , vec3 0 1 0
                        , vec3 0 0 1
                        ]
                in
                    boxHull 1
                        |> addEdgesOfConvexPolyhedron fullSeedSet
                        |> Expect.equal fullSeedSet
        , test "works for the box with negatively directed seeds" <|
            \_ ->
                let
                    fullSeedSet =
                        [ vec3 -1 0 0
                        , vec3 0 -1 0
                        , vec3 0 0 -1
                        ]
                in
                    boxHull 1
                        |> addEdgesOfConvexPolyhedron fullSeedSet
                        |> Expect.equal fullSeedSet
        , test "works for the box with partial seeds" <|
            \_ ->
                let
                    -- A partial seed set should get filled out by the addition of
                    -- complementary edges. This tests that the de-duping is not
                    -- wildly over- or under- aggressive.
                    partialSeedSet =
                        [ vec3 -1 0 0
                        , vec3 0 0 1
                        ]
                in
                    boxHull 1
                        |> addEdgesOfConvexPolyhedron partialSeedSet
                        |> List.length
                        |> Expect.equal 3
        , test "works for the box with different partial seeds" <|
            \_ ->
                let
                    -- A partial seed set should get filled out by the addition of
                    -- complementary edges. This tests that the de-duping is not
                    -- wildly over- or under- aggressive.
                    partialSeedSet =
                        [ vec3 0 0 1 ]
                in
                    boxHull 1
                        |> addEdgesOfConvexPolyhedron partialSeedSet
                        |> List.length
                        |> Expect.equal 3
        , test "works for the box with other different partial seeds" <|
            \_ ->
                let
                    -- A partial seed set should get filled out by the addition of
                    -- complementary edges. This tests that the de-duping is not
                    -- wildly over- or under- aggressive.
                    partialSeedSet =
                        [ vec3 0 1 0 ]
                in
                    boxHull 1
                        |> addEdgesOfConvexPolyhedron partialSeedSet
                        |> List.length
                        |> Expect.equal 3
        , test "works for the box with approximate seeds" <|
            \_ ->
                let
                    -- These approximate seeds should mask as effectively
                    -- as their exact integer equivalents would, ASSUMING
                    -- we have avoided the worst case scenario.
                    -- That would be when the vertices under test are
                    -- near the specific precision boundaries that would
                    -- cause insignificant error terms to compound
                    -- instead of canceling in the edge calculations.
                    validSeedSet =
                        [ vec3 (1 - Const.precision / 3.0) 0 0
                        , vec3 0 (1 + Const.precision / 3.0) 0
                        , vec3 0 0 ((-1) - Const.precision / 3.0)
                        ]
                in
                    boxHull 1
                        |> addEdgesOfConvexPolyhedron validSeedSet
                        |> Expect.equal validSeedSet
        , test "works for the box with invalid seeds" <|
            \_ ->
                let
                    -- Each invalid seed should simply linger in the result
                    -- with no effect on how (many) valid elements are added
                    -- as complementary edges. This tests that de-duping is not
                    -- overly aggressive in its matching.
                    -- Note: Some seeds use (Const.precision * 3.0) offsets to
                    -- force values that are purposely not quite precise enough
                    -- to match "exact" vertex values.
                    -- These tests should work as well with non-exact vertices
                    -- except in a worst case scenario: we are ASSUMING that
                    -- any insignificant error terms in the vertex values are
                    -- not cases that will be compounded by the edge
                    -- calculations in the same specific dimension as these
                    -- test offsets.
                    invalidSeedSet =
                        [ vec3 1 1 0
                        , vec3 (1 - Const.precision * 3.0) 0 0
                        , vec3 0 0 ((-1) - Const.precision * 3.0)
                        , vec3 0 0 0
                        ]
                in
                    boxHull 1
                        |> addEdgesOfConvexPolyhedron invalidSeedSet
                        |> List.length
                        |> Expect.equal (List.length invalidSeedSet + 3)

        -- The square pyramid shape has fewer parallel edges than a box.
        -- The extent of parallel edges in a box was masking a bug discovered
        -- in code review of addFaceEdges/uniqueEdges that would miss
        -- some edges.
        , test "works for a square pyramid" <|
            \_ ->
                squarePyramid
                    |> uniqueEdgesOfConvexPolyhedron
                    |> List.length
                    |> Expect.equal 6
        , test "works for a square pyramid with seeds" <|
            \_ ->
                let
                    partialSeedSet =
                        [ vec3 1 0 0
                        , vec3 0 1 0
                        ]
                in
                    squarePyramid
                        |> addEdgesOfConvexPolyhedron partialSeedSet
                        |> List.length
                        |> Expect.equal 6
        , test "works for an off-square pyramid" <|
            \_ ->
                askewSquarePyramid
                    |> uniqueEdgesOfConvexPolyhedron
                    |> List.length
                    |> Expect.equal 6
        , test "works for an off-square pyramid with seeds" <|
            \_ ->
                let
                    partialSeedSet =
                        [ vec3 1 0 0
                        , vec3 0 1 0
                        ]
                in
                    askewSquarePyramid
                        |> addEdgesOfConvexPolyhedron partialSeedSet
                        |> List.length
                        |> Expect.equal 6
        , test "works for a non-square-quad-based pyramid" <|
            \_ ->
                nonSquareQuadPyramid
                    |> uniqueEdgesOfConvexPolyhedron
                    |> List.length
                    |> Expect.equal 8

        -- all edges unique, none parallel
        , test "works for a non-square-quad-based pyramid with seeds" <|
            \_ ->
                let
                    partialSeedSet =
                        [ vec3 1 0 0
                        , vec3 0 1 0
                        ]
                in
                    nonSquareQuadPyramid
                        |> addEdgesOfConvexPolyhedron partialSeedSet
                        |> List.length
                        |> Expect.equal 8

        -- all edges unique, none parallel
        ]


boxUniqueEdges : Test
boxUniqueEdges =
    describe "ConvexPolyhedron.boxUniqueEdges"
        [ test "works for the box" <|
            \_ ->
                boxHull 1
                    |> .edges
                    |> Expect.equal
                        [ vec3 1 0 0
                        , vec3 0 1 0
                        , vec3 0 0 1
                        ]
        ]



-- Test helper functions


{-| Provide convenient test access to uniqueEdges based on the faces and
vertices of an existing ConvexPolyhedron. There is no need for this in
production, where uniqueEdges is called once at most per ConvexPolyhedron
and BEFORE that ConvexPolyhedron is fully initialized. Its result gets
cached in ConvexPolyhedron.edges.
-}
uniqueEdgesOfConvexPolyhedron : ConvexPolyhedron -> List Vec3
uniqueEdgesOfConvexPolyhedron { vertices, faces } =
    ConvexPolyhedron.uniqueEdges vertices faces


{-| This test helper function is intended as a more flexible variant of
ConvexPolyhedron.uniqueEdges. Its differences from uniqueEdges are that it can
be fed an initial list of "seed" edges and it operates on a pre-existing
ConvexPolyhedron's vertices and faces.
See the comment on uniqueEdgesOfConvexPolyhedron.
These differences have no application in production.
Keep this code in sync with any changes to ConvexPolyhedron.uniqueEdges.
-}
addEdgesOfConvexPolyhedron : List Vec3 -> ConvexPolyhedron -> List Vec3
addEdgesOfConvexPolyhedron seedEdges { vertices, faces } =
    faces
        |> Array.foldl
            (ConvexPolyhedron.addFaceEdges vertices)
            seedEdges



-- Test data generators


{-| A ConvexPolyhedron for a cube with the given half-extent, constructed
using optimized box-specific initializers.
-}
boxHull : Float -> ConvexPolyhedron
boxHull halfExtent =
    ConvexPolyhedron.fromBox (vec3 halfExtent halfExtent halfExtent)


{-| A replacement for boxhull/ConvexPolyhedron.fromBox that introduces some
minor imprecision into one of the box vertexes and can NOT be constructed
using optimized box-specific initializers -- except for the trivial boxFaces.
-}
boxyHull : Float -> ConvexPolyhedron
boxyHull halfExtent =
    let
        vertices =
            Array.fromList
                [ vec3 -halfExtent -halfExtent -halfExtent
                , vec3 halfExtent -halfExtent -halfExtent
                , vec3 halfExtent halfExtent -halfExtent
                , vec3 -halfExtent halfExtent -halfExtent
                , vec3 -halfExtent -halfExtent halfExtent
                , vec3 halfExtent -halfExtent halfExtent

                -- Insignificantly adjust two vertex coordinates to force the 3
                -- connected edges to be insignificantly off-parallel.
                -- This should NOT alter the number of uniqueEdges
                , vec3 halfExtent (halfExtent - Const.precision / 3.0) (halfExtent + Const.precision / 3.0)
                , vec3 -halfExtent halfExtent halfExtent
                ]

        faces =
            ConvexPolyhedron.boxFaces
    in
        { faces = faces
        , facesLength = 6
        , vertices = vertices

        -- To test the handling of minor imprecision in a general
        -- ConvexPolyhedron, purposely bypass the box-specific
        -- optimizations in boxNormals and boxEdges and use instead
        -- the general purpose calculations.
        , normals = ConvexPolyhedron.faceNormals vertices faces
        , edges = ConvexPolyhedron.uniqueEdges vertices faces
        }


squarePyramid : ConvexPolyhedron
squarePyramid =
    -- Specify 0 for exact precision
    squareLikePyramid 0.0


askewSquarePyramid : ConvexPolyhedron
askewSquarePyramid =
    -- Use an insignificant epsilon for an approximately square base
    squareLikePyramid (Const.precision / 3.0)


nonSquareQuadPyramid : ConvexPolyhedron
nonSquareQuadPyramid =
    -- Use a significant epsilon for a not even approximately square base
    squareLikePyramid (Const.precision * 3.0)


squareLikePyramid : Float -> ConvexPolyhedron
squareLikePyramid epsilon =
    let
        x =
            1

        y =
            1

        z =
            1

        -- zOffset is the height of the pyramid's center of gravity above its
        -- base -- the cube root of 1/2.
        -- It serves to keep the object vertically centered.
        zOffset =
            z * (0.5 ^ (1.0 / 3.0))

        faces =
            Array.fromList
                [ Array.fromList [ 3, 2, 1, 0 ]
                , Array.fromList [ 0, 1, 4 ]
                , Array.fromList [ 1, 2, 4 ]
                , Array.fromList [ 2, 3, 4 ]
                , Array.fromList [ 3, 0, 4 ]
                ]

        vertices =
            Array.fromList
                [ vec3 -x -y -zOffset
                , vec3 x -y -zOffset

                -- An optional adjustment of one base corner controls
                -- the number (0 or 2) of edge pairs that are exactly
                -- parallel OR approximately parallel.
                , vec3 (x + epsilon) (y + epsilon) -zOffset
                , vec3 -x y -zOffset
                , vec3 0 0 (z - zOffset)
                ]
    in
        { faces = faces
        , facesLength = 5
        , vertices = vertices
        , normals = ConvexPolyhedron.faceNormals vertices faces
        , edges = ConvexPolyhedron.uniqueEdges vertices faces
        }
