namespace BigDataCounting

module Bitmap =

   type ByteArray = byte[]
   type ByteSeq = seq<byte>

   /// Provides a sequence which results from a bitwise OR
   /// between corresponding elements from the input sequences.
   let Or (m1 : ByteSeq) (m2 : ByteSeq) : ByteSeq =
      Seq.combine (|||) m1 m2

   /// Provides a sequence which results from a bitwise AND
   /// between corresponding elements from the input sequences.
   let And (m1 : ByteSeq) (m2 : ByteSeq) : ByteSeq =
      Seq.combine (&&&) m1 m2

   /// Applies a bitwise OR between corresponding bytes in all the
   /// input sequences and returns a byte sequence of the results.
   let AllOr (ms : seq<ByteSeq>) =
      if ms |> Seq.isEmpty then
         Seq.empty
      else
         ms |> Seq.reduce Or

   /// A sequence of bytes 0000 0000, 0000 0001, 0000 0010... 1111 1111.
   let private Masks = 
      [| for i in 0..7 -> pown 2 i |> byte |]

   /// Counts the number of 1-bits in a byte.
   let OnesInByte (by : byte) =
      Masks |> Seq.countWith (fun m -> by &&& m > 0uy)

   /// Counts the number of 1-bits in a sequence of bytes.
   let OneCount (s : ByteSeq) =
      s |> Seq.sumBy OnesInByte

   /// Sets the bit at the specified bit-level offset in the
   /// provided array of bytes.
   //
   // Using a uint32 here gives us a bit of extra space
   // but will fail at runtime when index / 8 > MaxInt32
   let SetBit (bytes : ByteArray) (index : uint32) =
      let byteIndex = index / 8u |> int
      let bitIndex = index % 8u |> int
      let currentByte = bytes.[byteIndex]
      let mask = pown 2 bitIndex |> byte
      bytes.[byteIndex] <- currentByte ||| mask

