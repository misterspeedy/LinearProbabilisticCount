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
   let s1, s2 = [ 0b10101010uy; 0b01010101uy ], [ 0b01010101uy; 0b10101010uy ]
   let actual = Bitmap.And s1 s2
   actual |> should equal expected

// AllOr

[<Test>]
let ``When provided with an empty sequence AllOr returns an empty sequence``() =
   let expected = Seq.empty
   let actual = Seq.empty |> Bitmap.AllOr
   actual |> should equal expected

[<Test>]
let ``When provided with a sequence of sequences containing one 0 AllOr returns a sequence of one 0``() =
   let zerosSeq = [0uy] |> Seq.ofList
   let expected = zerosSeq 
   let actual = [zerosSeq] |> Seq.ofList |> Bitmap.AllOr
   actual |> should equal expected

[<Test>]
let ``When provided with a sequence of sequences containing one 255 AllOr returns a sequence of one 255``() =
   let onesSeq = [255uy] |> Seq.ofList
   let expected = onesSeq 
   let actual = [onesSeq] |> Seq.ofList |> Bitmap.AllOr
   actual |> should equal expected

[<Test>]
let ``When provided with a sequence of sequences containing one 255 and one 0 AllOr returns a sequence of one 255``() =
   let zerosSeq = [0uy] |> Seq.ofList
   let onesSeq = [255uy] |> Seq.ofList
   let expected = onesSeq 
   let actual = [onesSeq; zerosSeq] |> Seq.ofList |> Bitmap.AllOr
   actual |> should equal expected

[<Test>]
let ``When provided with a sequence of sequences containing interleaved bits AllOr returns a sequence of 255s``() =
   let odd = [0b01010101uy; 0b01010101uy] |> Seq.ofList
   let even = [0b10101010uy; 0b10101010uy] |> Seq.ofList
   let expected = [255uy; 255uy] |> Seq.ofList 
   let actual = [odd; even] |> Seq.ofList |> Bitmap.AllOr
   actual |> should equal expected

// OnesInByte

[<TestCase(0b00000000uy, 0)>]
[<TestCase(0b00000001uy, 1)>]
[<TestCase(0b00000010uy, 1)>]
[<TestCase(0b00000011uy, 2)>]
[<TestCase(0b10000000uy, 1)>]
[<TestCase(0b11111110uy, 7)>]
[<TestCase(0b11111111uy, 8)>]
let ``OnesInByte returns the correct count for a variety of inputs``(input, expected) =
   let actual = input |> Bitmap.OnesInByte
   actual |> should equal expected

// OneCount

[<Test>]
let ``When provided with an empty sequence OneCount returns 0``() =
   let expected = 0
   let actual = Seq.empty |> Bitmap.OneCount
   actual |> should equal expected

[<Test>]
let ``When provided with a sequence consisting of 0 bytes OneCount returns 0``() =
   let expected = 0
   let zeros = Seq.init 10 (fun _ -> 0uy)
   let actual = zeros|> Bitmap.OneCount
   actual |> should equal expected

[<Test>]
let ``When provided with a sequence consisting of ten x 255 bytes OneCount returns 10 x 8``() =
   let expected = 80
   let two55s = Seq.init 10 (fun _ -> 255uy)
   let actual = two55s|> Bitmap.OneCount
   actual |> should equal expected

[<Test>]
let ``When provided with a sequence consisting of ten x 00000001 bytes OneCount returns 10``() =
   let expected = 10
   let two55s = Seq.init 10 (fun _ -> 1uy)
   let actual = two55s|> Bitmap.OneCount
   actual |> should equal expected

[<Test>]
let ``When provided with a sequence consisting of ten x 10000001 bytes OneCount returns 20``() =
   let expected = 20
   let bytes = Seq.init 10 (fun _ -> 0b10000001uy)
   let actual = bytes |> Bitmap.OneCount
   actual |> should equal expected

[<Test>]
let ``When provided with a sequence consisting of arbitrary bytes OneCount returns the correct result``() =
   let expected = 35
   let bytes = [0b00000001uy; 0b00101000uy; 0b11110101uy; 0b10010101uy; 0b11110110uy; 
                0b00101011uy; 0b11111110uy; 0b01000000uy; 0b00000000uy; 0b10101010uy ] 
   let actual = bytes |> Bitmap.OneCount
   actual |> should equal expected

