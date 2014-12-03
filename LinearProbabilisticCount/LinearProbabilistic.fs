namespace BigDataCounting

open FSharp.Collections.ParallelSeq

module LinearProbabilistic =

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

   /// Produces a linear probabilistic count of the unique items
   /// in the given sequence, using the specified size for the
   /// bitmap.  Accuracy increases with larger map sizes.
   ///
   /// http://highscalability.com/blog/2012/4/5/big-data-counting-how-to-count-a-billion-distinct-objects-us.html
   let private CreateMap (mapSizeBits : int) (s : seq<'T>) =
      let map = Array.zeroCreate (mapSizeBits*8)

      s
      |> Seq.map (WrappedHash (uint32 mapSizeBits))
      |> Seq.iter (Bitmap.SetBit map)

      map

   let private EstimateFromMap map = 
      let w = map |> Bitmap.OneCount |> double
      // TODO avoid recalculating length
      let m = map |> Seq.length |> double // TODO why is this right? Should be length in bits????
      -m * System.Math.Log((m-w)/m)

   // TODO remove mapSizeBytes/Bits confusion and always work in bits
   let Count (mapSizeBytes : int) (s : seq<'T>) =
      let mapSizeBits = mapSizeBytes * 8
      let map = CreateMap mapSizeBits s
      EstimateFromMap map

   let PCount (mapSizeBytes : int) (parallelism : int) (s : seq<'T>) =
      let mapSizeBits = mapSizeBytes * 8
   
      // Divide the sequence up into n sections:
      let numbered =
         s |> Seq.mapi (fun i x -> i, x)
      let sections =
         [0..parallelism-1]
         |> PSeq.map (fun x -> numbered |> Seq.filter (fun (i, _) -> i % parallelism = x) |> Seq.map snd)

      // Separately and in parallel compute the bitmaps.
      // OR all the bitmaps together:
      let map =
         sections 
         |> PSeq.map (fun section -> section |> CreateMap mapSizeBits |> Seq.ofArray)
         |> Bitmap.AllOr

      // Apply the standard estimation formula:
      EstimateFromMap map

