namespace BigDataCounting

open FSharp.Collections.ParallelSeq

module LinearProbabilistic =

   [<Measure>]
   type Bit
   [<Measure>]
   type Byte

   /// Calculates the number of whole bytes required to accommodate the specified number of bits.
   let bitsToBytes (bits : int<Bit>) : int<Byte> = 
      double(bits) / 8. 
      |> System.Math.Ceiling 
      |> int 
      |> LanguagePrimitives.Int32WithMeasure

   /// Gets the hash code of an object but cast to uint32.
   /// Has a higher probability of collisions than the pure hash.
   let Uint32Hash o =
      o.GetHashCode() |> uint32

   /// Gets the hash code of an object but 'wrapped' using modulus
   /// when the code exceeds the stated maximum. 
   /// Has a higher probability of collisions than the pure hash.
   let WrappedHash max o =
      if max = 0u then
         raise (new System.ArgumentException("Max must be > 0"))
      else
         (o |> Uint32Hash) % max

   let private CreateMap (mapSize : int<Bit>) (s : seq<'T>) =
      let map = Array.zeroCreate (mapSize |> bitsToBytes |> int)

      s
      |> Seq.map (WrappedHash (mapSize |> uint32))
      |> Seq.iter (Bitmap.SetBit map)

      map

   let private EstimateFromMap map = 
      let w = map |> Bitmap.OneCount |> double
      // TODO avoid recalculating length
      let m = (map |> Seq.length |> double) * 8.
      -m * System.Math.Log((m-w)/m) |> int

   /// Produces a linear probabilistic count of the unique items
   /// in the given sequence, using the specified size for the
   /// bitmap.  Accuracy increases with larger map sizes.
   ///
   /// http://highscalability.com/blog/2012/4/5/big-data-counting-how-to-count-a-billion-distinct-objects-us.html
   let Count (mapSize : int<Bit>) (s : seq<'T>) =
      let map = CreateMap mapSize s
      EstimateFromMap map

   /// Produces a linear probabilistic count of the unique items
   /// in the given sequence, using the specified size for the
   /// bitmap.  Accuracy increases with larger map sizes. Uses
   /// the FSharp parallel sequence library to parallelise the
   /// calculation.
   ///
   /// http://highscalability.com/blog/2012/4/5/big-data-counting-how-to-count-a-billion-distinct-objects-us.html
   let PCount (mapSize : int<Bit>) (parallelism : int) (s : seq<'T>) =
      // Divide the sequence up into n sections:
      let numbered =
         s |> Seq.mapi (fun i x -> i, x)

      // Separately and in parallel compute the bitmaps, then
      // OR all the resulting bitmaps together:
      let map =
         [0..parallelism-1]
         |> PSeq.map (fun x -> numbered |> Seq.filter (fun (i, _) -> i % parallelism = x) |> Seq.map snd)
         |> PSeq.map (fun section -> section |> CreateMap mapSize |> Seq.ofArray)
         |> Bitmap.AllOr

      // Apply the standard estimation formula:
      EstimateFromMap map

