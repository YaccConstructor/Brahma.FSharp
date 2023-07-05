module Brahma.FSharp.Tests.Translator.LambdaLifting.Tests

open Expecto
open Brahma.FSharp
open Brahma.FSharp.Tests.Translator.Common

let lambdaLiftingTests translator = [
    let inline checkCode cmd outFile expected = checkCode translator cmd outFile expected

    testCase "Template Let Transformation Test 0" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f = 3
                buf.[0] <- f
            @>

        checkCode command "Template Test 0.gen" "Template Test 0.cl"

    testCase "Template Let Transformation Test 1" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f =
                    let x = 3
                    x

                buf.[0] <- f
            @>

        checkCode command "Template Test 1.gen" "Template Test 1.cl"

    testCase "Template Let Transformation Test 2" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f =
                    let x =
                        let y = 3
                        y

                    x

                buf.[0] <- f
            @>

        checkCode command "Template Test 2.gen" "Template Test 2.cl"

    testCase "Template Let Transformation Test 3" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f =
                    let f = 5
                    f

                buf.[0] <- f
            @>

        checkCode command "Template Test 3.gen" "Template Test 3.cl"

    testCase "Template Let Transformation Test 4" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f =
                    let f =
                        let f = 5
                        f

                    f

                buf.[0] <- f
            @>

        checkCode command "Template Test 4.gen" "Template Test 4.cl"

    testCase "Template Let Transformation Test 5" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f a b =
                    let x y z = y + z
                    x a b

                buf.[0] <- f 1 7
            @>

        checkCode command "Template Test 5.gen" "Template Test 5.cl"

    testCase "Template Let Transformation Test 6" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f x y =
                    let x = x
                    x + y

                buf.[0] <- f 7 8
            @>

        checkCode command "Template Test 6.gen" "Template Test 6.cl"

    testCase "Template Let Transformation Test 7" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f y =
                    let x y = 6 - y
                    x y

                buf.[0] <- f 7
            @>

        checkCode command "Template Test 7.gen" "Template Test 7.cl"

    testCase "Template Let Transformation Test 8" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (m: int clarray) ->
                let p = m.[0]

                let x n =
                    let l = m.[9]
                    let g k = k + m.[0] + m.[1]

                    let r =
                        let y a =
                            let x = 5 - n + (g 4)
                            let z t = m.[2] + a - t
                            z (a + x + l)

                        y 6

                    r + m.[3]

                m.[0] <- x 7
            @>

        checkCode command "Template Test 8.gen" "Template Test 8.cl"

    testCase "Template Let Transformation Test 9" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let x n =
                    let r = 8
                    let h = r + n
                    h

                buf.[0] <- x 9
            @>

        checkCode command "Template Test 9.gen" "Template Test 9.cl"

    testCase "Template Let Transformation Test 10" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let p = 9

                let x n b =
                    let t = 0
                    n + b + t

                buf.[0] <- x 7 9
            @>

        checkCode command "Template Test 10.gen" "Template Test 10.cl"

    testCase "Template Let Transformation Test 11" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let p = 1

                let m =
                    let r l = l + p
                    r 9

                let z k = k + 1
                buf.[0] <- m
            @>

        checkCode command "Template Test 11.gen" "Template Test 11.cl"

    testCase "Template Let Transformation Test 12" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f x y =
                    let y = y
                    let y = y
                    let g x m = m + x
                    g x y

                buf.[0] <- f 1 7
            @>

        checkCode command "Template Test 12.gen" "Template Test 12.cl"

    testCase "Template Let Transformation Test 13" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f y =
                    let y = y
                    let y = y
                    let g m = m + 1
                    g y

                buf.[0] <- f 7
            @>

        checkCode command "Template Test 13.gen" "Template Test 13.cl"

    testCase "Template Let Transformation Test 14" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f (y: int) =
                    let y = y
                    let y = y

                    let g (m: int) =
                        let g r t = r + y - t
                        let n o = o - (g y 2)
                        n 5

                    g y

                let z y = y - 2
                buf.[0] <- f (z 7)
            @>

        checkCode command "Template Test 14.gen" "Template Test 14.cl"

    testCase "Template Let Transformation Test 15" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f y =
                    let Argi index = if index = 0 then buf.[1] else buf.[2]
                    Argi y

                buf.[0] <- f 0
            @>

        checkCode command "Template Test 15.gen" "Template Test 15.cl"

    testCase "Template Let Transformation Test 16" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f y =
                    if y = 0 then
                        let z a = a + 1
                        z 9
                    else
                        buf.[2]

                buf.[0] <- f 0
            @>

        checkCode command "Template Test 16.gen" "Template Test 16.cl"

    testCase "Let renamed" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f x =
                    let g = 1 + x
                    g

                buf.[0] <- f 1
            @>

        checkCode command "Let renamed.gen" "Let renamed.cl"

    testCase "Let renamed 2" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f m k =
                    let g q w = 1 + q + w
                    let t p = 7 - p
                    (g 1 2) - m * k / (t 53)

                buf.[0] <- f 1 4
            @>

        checkCode command "Let renamed 2.gen" "Let renamed 2.cl"

    testCase "Renamer Test" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f x y =
                    let y = y
                    let y = y
                    let g x m = m + x
                    g x y

                buf.[0] <- f 1 7
            @>

        checkCode command "Renamer Test.gen" "Renamer Test.cl"

    testCase "Nested functions" <| fun _ ->
        let command =
            <@ fun (range: Range1D) (buf: int clarray) ->
                let f x y = x - y
                buf.[0] <- f 2 3
                buf.[1] <- f 4 5
            @>

        checkCode command "Nested.Function.gen" "Nested.Function.cl"
]
