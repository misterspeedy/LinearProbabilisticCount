[<NUnit.Framework.TestFixture>]
module LinearProbabalisticTests

open NUnit.Framework
open FsUnit
open BigDataCounting

// Uint32Hash 

[<TestCase(0, 0u)>]
[<TestCase(1, 1u)>]
[<TestCase(-1, 4294967295u)>]
let ``Uint32Hash takes an int32 hash and produces an equivalent uint32 hash``(int32Hash, expected) =
   let actual = int32Hash |> LinearProbabilistic.Uint32Hash
   actual |> should equal expected

// WrappedHash

[<TestCase(0, 10u, 0u)>]
[<TestCase(1, 10u, 1u)>]
[<TestCase(1000000, 1000001u, 1000000u)>]
let ``WrappedHash takes an int32 hash and produces an equivalent uint32 hash when the stated max is not reached``(int32Hash, max, expected) =
   let actual = int32Hash |> LinearProbabilistic.WrappedHash max
   actual |> should equal expected

[<TestCase(10, 10u, 0u)>]
[<TestCase(11, 10u, 1u)>]
[<TestCase(1000000, 1000000u, 0u)>]
[<TestCase(1000002, 1000000u, 2u)>]
[<TestCase(2000002, 1000000u, 2u)>]
let ``WrappedHash takes an int32 hash and produces an equivalent uint32 hash when the stated max is equalled``(int32Hash, max, expected) =
   let actual = int32Hash |> LinearProbabilistic.WrappedHash max
   actual |> should equal expected

[<Test>]
let ``WrappedHash raises an exception when provided with a max of zero``() =
   let cant = (fun () -> 99 |> LinearProbabilistic.WrappedHash 0u |> ignore)
   cant |> should throw typeof<System.ArgumentException>

// Count

let private GuidsSequence count =
   seq {
      for _ in 1..count do
         yield System.Guid.NewGuid()
   }

let private GuidsSequenceRepeated cardinality count =
   let r = new System.Random(1)
   if count % cardinality <> 0 then
      raise (new System.ArgumentException("Count must be divisible by cardinality"))

   let innerSeq = GuidsSequence cardinality |> Seq.cache

   seq {
      for _ in 1..count/cardinality -> innerSeq
   }
   |> Seq.concat
   |> Seq.sortBy (fun _ -> r.Next())

[<TestCase(1000, 10000, 1E-3, 4)>]
[<TestCase(1000, 1000, 1E-2, 4)>]
[<TestCase(1000, 100, 1E-1, 4)>]
[<TestCase(10000, 10000, 1E-1, 100)>]
[<TestCase(10000, 1000, 5.0, 100)>]
let ``Given a sequence with a given cardinality and a specific map size Count produces an estimate within a specific margin``(totalCount, mapSize, margin, expected) =
   let cardinality = expected
   let guids = GuidsSequenceRepeated cardinality (totalCount * cardinality)
   let actual = guids |> LinearProbabilistic.Count mapSize 
   actual |> should (equalWithin margin) expected

// PCount

[<TestCase(1000, 10000, 1E-3, 8, 4)>]
[<TestCase(1000, 1000, 1E-2, 8, 4)>]
[<TestCase(1000, 100, 1E-1, 8, 4)>]
[<TestCase(10000, 10000, 1E-1, 8, 100)>]
[<TestCase(10000, 1000, 5.0, 8, 100)>]
let ``Given a sequence with a given cardinality map size and degree of parallelism PCount produces an estimate within a specific margin``(totalCount, mapSize, margin, parallelism, expected) =
   let cardinality = expected
   let guids = GuidsSequenceRepeated cardinality (totalCount * cardinality)
   let actual = guids |> LinearProbabilistic.PCount mapSize parallelism
   actual |> should (equalWithin margin) expected



// TODO have a specific exception message for map size too small instead of infinity
// TODO return an accuracy estimate as part of the return results?

// FSharp.Collections.ParallelSeq  |> PSEq



