namespace BigDataCounting

module LinearProbabilistic =

   /// Gets the hash code of an object but cast to uint32.
   /// Has a higher probability of collisions than the pure hash.
   let Uint32Hash o =
      o.GetHashCode() |> uint32

   /// Gets the hash code of an object but 'wrapped' using modulus
   /// when the code exceeds the stated maximum. 
   /// Has a higher probability of collisions than the pure hash.
   let WrappedHash max o =
      (o |> Uint32Hash) % max

   /// Produces a linear probabilistic count of the unique items
   /// in the given sequence, using the specified size for the
   /// bitmap.  Accuracy increases with larger map sizes.
   ///
   /// http://highscalability.com/blog/2012/4/5/big-data-counting-how-to-count-a-billion-distinct-objects-us.html
   let Count (mapSizeBytes : int) (s : seq<'T>) =
      let map = Array.zeroCreate mapSizeBytes

      let mapSizeBits = mapSizeBytes * 8

      s
      |> Seq.map (WrappedHash (uint32 mapSizeBits))
      |> Seq.iter (Bitmap.SetBit map)

      let w = map |> Bitmap.OneCount |> double
      let m = mapSizeBits |> double

      -m * System.Math.Log((m-w)/m)

