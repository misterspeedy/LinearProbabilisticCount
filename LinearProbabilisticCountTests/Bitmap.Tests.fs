[<NUnit.Framework.TestFixture>]
module BitmapTests

open NUnit.Framework
open FsUnit
open BigDataCounting

// Seq.Or

[<Test>]
let ``When provided with two empty sequences Bitmap.Or returns an empty sequence``() =
   let expected = 0
   let s1, s2 = Seq.empty, Seq.empty
   let actual = Bitmap.Or s1 s2 |> Seq.length
   actual |> should equal expected

[<Test>]
let ``When provided with two sequences of 0s Bitmap.Or returns a sequence of 0s of the same length``() =
   let expected = Seq.init 10 (fun _ -> 0uy)
   let s1, s2 = Seq.init 10 (fun _ -> 0uy), Seq.init 10 (fun _ -> 0uy)
   let actual = Bitmap.Or s1 s2
   actual |> should equal expected

[<Test>]
let ``When provided with two sequences of 1s Bitmap.Or returns a sequence of 1s of the same length``() =
   let expected = Seq.init 10 (fun _ -> 1uy)
   let s1, s2 = Seq.init 10 (fun _ -> 1uy), Seq.init 10 (fun _ -> 1uy)
   let actual = Bitmap.Or s1 s2
   actual |> should equal expected

[<Test>]
let ``When provided with two complementary sequences Bitmap.Or returns a sequence of 255s of the same length``() =
   let expected = Seq.init 255 (fun _ -> 255uy)
   let s1 = Seq.init 255 (fun i -> i |> byte) |> Array.ofSeq
   let s2 = Seq.init 255 (fun i -> 255-i |> byte) |> Array.ofSeq
   let actual = Bitmap.Or s1 s2
   actual |> should equal expected

// Seq.And

[<Test>]
let ``When provided with two empty sequences Bitmap.And returns an empty sequence``() =
   let expected = 0
   let s1, s2 = Seq.empty, Seq.empty
   let actual = Bitmap.And s1 s2 |> Seq.length
   actual |> should equal expected

[<Test>]
let ``When provided with two sequences of 0s Bitmap.And returns a sequence of 0s of the same length``() =
   let expected = Seq.init 10 (fun _ -> 0uy)
   let s1, s2 = Seq.init 10 (fun _ -> 0uy), Seq.init 10 (fun _ -> 0uy)
   let actual = Bitmap.And s1 s2
   actual |> should equal expected

[<Test>]
let ``When provided with two sequences of 1s Bitmap.And returns a sequence of 1s of the same length``() =
   let expected = Seq.init 10 (fun _ -> 1uy)
   let s1, s2 = Seq.init 10 (fun _ -> 1uy), Seq.init 10 (fun _ -> 1uy)
   let actual = Bitmap.And s1 s2
   actual |> should equal expected

[<Test>]
let ``When provided a sequence of 1s and a sequence of 0s Bitmap.And returns a sequence of 0s of the same length``() =
   let expected = Seq.init 10 (fun _ -> 0uy)
   let s1, s2 = Seq.init 10 (fun _ -> 1uy), Seq.init 10 (fun _ -> 0uy)
   let actual = Bitmap.And s1 s2
   actual |> should equal expected

[<Test>]
let ``When provided with interleaved 0s and 1s Bitmap.And returns a sequence of 0s of the same length``() =
   let expected = Seq.init 2 (fun _ -> 0uy)
   let s1, s2 = [ 170uy; 85uy ], [ 85uy;  170uy ]
   let actual = Bitmap.And s1 s2
   actual |> should equal expected
