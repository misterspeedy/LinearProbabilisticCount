[<NUnit.Framework.TestFixture>]
module LinearProbabilisticCountTests

open NUnit.Framework
open FsUnit
open BigDataCounting

// Seq.combine

[<Test>]
let ``When provided with two empty sequences Seq.combine returns an empty sequence``() =
   let expected = 0
   let s1, s2 = Seq.empty, Seq.empty
   let actual = Seq.combine id s1 s2 |> Seq.length
   actual |> should equal expected

[<Test>]
let ``When provided with two non-empty sequence Seq.combine returns an the result of applying the supplied function to corresponding pairs``() =
   let expected = seq {for i in 0..9 -> i + pown i i}
   let s1 = {0..9}
   let s2 = seq {for i in 0..9 -> pown i i}
   let actual = Seq.combine (+) s1 s2
   actual |> should equal expected 
   
// Seq.countWith

[<Test>]
let ``When provided with an empty sequence Seq.countWith returns 0``() =
   let expected = 0
   let s = Seq.empty
   let actual = s |> Seq.countWith (fun _ -> true)
   actual |> should equal expected 
   
[<Test>]
let ``When provided with a non-empty sequence and a function which returns false Seq.countWith returns 0``() =
   let expected = 0
   let s = {0..999}
   let actual = s |> Seq.countWith (fun _ -> false)
   actual |> should equal expected 

[<Test>]
let ``When provided with a non-empty sequence and a function which returns true Seq.countWith returns the original length``() =
   let expected = 1000
   let s = {0..999}
   let actual = s |> Seq.countWith (fun _ -> true)
   actual |> should equal expected 

[<Test>]
let ``When provided with a non-empty sequence and a meaningful function Seq.countWith returns the count of trues returned``() =
   let expected = 500
   let s = {0..999}
   let actual = s |> Seq.countWith (fun x -> x % 2 = 0)
   actual |> should equal expected 




   
    