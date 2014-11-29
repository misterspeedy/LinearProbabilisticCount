namespace BigDataCounting

module Seq =

   /// Takes two sequences, zips them, and applies the supplied function
   /// to each pair, returning a sequence of the results.
   let combine f s1 s2 =
      Seq.zip s1 s2
      |> Seq.map (fun (v1, v2) -> f v1 v2)

   /// Counts the number of items in a sequence where f returns true for
   /// a sequence element.
   let countWith f s =
      s |> Seq.filter f |> Seq.length