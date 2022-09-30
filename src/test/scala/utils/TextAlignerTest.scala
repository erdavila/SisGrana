package sisgrana
package utils

import utils.TextAligner.{AlignedChunk, Alignment, Chunk, ConflictingChunksException, InvalidCenterAlignmentException}

class TextAlignerTest extends TestBase {
  test(".align()") {
    val cases = Table(
      "chunks" -> "expectedOutput",

      Seq.empty -> Seq.empty,

      Seq(Seq.empty) -> Seq(Seq.empty),

      //  |<--
      Seq(
        Seq(Chunk("xyz", Alignment.Left(1))),
      ) -> Seq(
        Seq(AlignedChunk(Chunk("xyz", Alignment.Left(1)), column = 0, leftPaddingWidth = 0)),
      ),

      //  -->|
      Seq(
        Seq(Chunk("xyz", Alignment.Right(1))),
      ) -> Seq(
        Seq(AlignedChunk(Chunk("xyz", Alignment.Right(1)), column = 0, leftPaddingWidth = 0)),
      ),

      //  |<-->|
      Seq(
        Seq(Chunk("xyz", Alignment.Center(1, 2))),
      ) -> Seq(
        Seq(AlignedChunk(Chunk("xyz", Alignment.Center(1, 2)), column = 0, leftPaddingWidth = 0)),
      ),

      //  |<-- |<--
      Seq(
        Seq(
          Chunk("xxx", Alignment.Left(1)),
          Chunk("yyyy", Alignment.Left(2)),
        ),
      ) -> Seq(
        Seq(
          AlignedChunk(Chunk("xxx", Alignment.Left(1)), column = 0, leftPaddingWidth = 0),
          AlignedChunk(Chunk("yyyy", Alignment.Left(2)), column = 3, leftPaddingWidth = 0),
        ),
      ),

      //  |<-- -->|
      Seq(
        Seq(
          Chunk("xxx", Alignment.Left(1)),
          Chunk("yyyy", Alignment.Right(2)),
        ),
        ) -> Seq(
        Seq(
          AlignedChunk(Chunk("xxx", Alignment.Left(1)), column = 0, leftPaddingWidth = 0),
          AlignedChunk(Chunk("yyyy", Alignment.Right(2)), column = 3, leftPaddingWidth = 0),
        ),
      ),

      //  |<-- |<-->|
      Seq(
        Seq(
          Chunk("xxx", Alignment.Left(1)),
          Chunk("yyyy", Alignment.Center(2, 3)),
        ),
      ) -> Seq(
        Seq(
          AlignedChunk(Chunk("xxx", Alignment.Left(1)), column = 0, leftPaddingWidth = 0),
          AlignedChunk(Chunk("yyyy", Alignment.Center(2, 3)), column = 3, leftPaddingWidth = 0),
        ),
      ),

      //  |<--
      //  |<--
      Seq(
        Seq(Chunk("xxx", Alignment.Left(1))),
        Seq(Chunk("yyyy", Alignment.Left(1))),
      )-> Seq(
        Seq(AlignedChunk(Chunk("xxx", Alignment.Left(1)), column = 0, leftPaddingWidth = 0)),
        Seq(AlignedChunk(Chunk("yyyy", Alignment.Left(1)), column = 0, leftPaddingWidth = 0)),
      ),

      //     |<--
      //  -->|
      Seq(
        Seq(Chunk("xxx", Alignment.Left(1))),
        Seq(Chunk("yyyy", Alignment.Right(1))),
      ) -> Seq(
        Seq(AlignedChunk(Chunk("xxx", Alignment.Left(1)), column = 4, leftPaddingWidth = 4)),
        Seq(AlignedChunk(Chunk("yyyy", Alignment.Right(1)), column = 0, leftPaddingWidth = 0)),
      ),

      //  |<--
      //  |<-->|
      Seq(
        Seq(Chunk("xxx", Alignment.Left(1))),
        Seq(Chunk("yyyy", Alignment.Center(1, 2))),
      ) -> Seq(
        Seq(AlignedChunk(Chunk("xxx", Alignment.Left(1)), column = 0, leftPaddingWidth = 0)),
        Seq(AlignedChunk(Chunk("yyyy", Alignment.Center(1, 2)), column = 0, leftPaddingWidth = 0)),
      ),

      //       |<--
      //  |<-->|
      Seq(
        Seq(Chunk("xxx", Alignment.Left(2))),
        Seq(Chunk("yyyy", Alignment.Center(1, 2))),
      ) -> Seq(
        Seq(AlignedChunk(Chunk("xxx", Alignment.Left(2)), column = 4, leftPaddingWidth = 4)),
        Seq(AlignedChunk(Chunk("yyyy", Alignment.Center(1, 2)), column = 0, leftPaddingWidth = 0)),
      ),

      //  -->|<--
      Seq(
        Seq(
          Chunk("xxx", Alignment.Right(1)),
          Chunk("yyyy", Alignment.Left(1)),
        ),
      ) -> Seq(
        Seq(
          AlignedChunk(Chunk("xxx", Alignment.Right(1)), column = 0, leftPaddingWidth = 0),
          AlignedChunk(Chunk("yyyy", Alignment.Left(1)), column = 3, leftPaddingWidth = 0),
        ),
      ),

      //  -->| |<--
      Seq(
        Seq(
          Chunk("xxx", Alignment.Right(1)),
          Chunk("yyyy", Alignment.Left(2)),
        ),
      ) -> Seq(
        Seq(
          AlignedChunk(Chunk("xxx", Alignment.Right(1)), column = 0, leftPaddingWidth = 0),
          AlignedChunk(Chunk("yyyy", Alignment.Left(2)), column = 3, leftPaddingWidth = 0),
        ),
      ),

      //  -->| -->|
      Seq(
        Seq(
          Chunk("xxx", Alignment.Right(1)),
          Chunk("yyyy", Alignment.Right(2)),
        ),
      ) -> Seq(
        Seq(
          AlignedChunk(Chunk("xxx", Alignment.Right(1)), column = 0, leftPaddingWidth = 0),
          AlignedChunk(Chunk("yyyy", Alignment.Right(2)), column = 3, leftPaddingWidth = 0),
        ),
      ),

      //  -->|<-->|
      Seq(
        Seq(
          Chunk("xxx", Alignment.Right(1)),
          Chunk("yyyy", Alignment.Center(1, 2)),
        ),
      ) -> Seq(
        Seq(
          AlignedChunk(Chunk("xxx", Alignment.Right(1)), column = 0, leftPaddingWidth = 0),
          AlignedChunk(Chunk("yyyy", Alignment.Center(1, 2)), column = 3, leftPaddingWidth = 0),
        ),
      ),

      //  -->| |<-->|
      Seq(
        Seq(
          Chunk("xxx", Alignment.Right(1)),
          Chunk("yyyy", Alignment.Center(2, 3)),
        ),
      ) -> Seq(
        Seq(
          AlignedChunk(Chunk("xxx", Alignment.Right(1)), column = 0, leftPaddingWidth = 0),
          AlignedChunk(Chunk("yyyy", Alignment.Center(2, 3)), column = 3, leftPaddingWidth = 0),
        ),
      ),

      //  -->|
      //     |<--
      Seq(
        Seq(Chunk("xxx", Alignment.Right(1))),
        Seq(Chunk("yyyy", Alignment.Left(1))),
      ) -> Seq(
        Seq(AlignedChunk(Chunk("xxx", Alignment.Right(1)), column = 0, leftPaddingWidth = 0)),
        Seq(AlignedChunk(Chunk("yyyy", Alignment.Left(1)), column = 3, leftPaddingWidth = 3)),
      ),

      //  -->|
      //  -->|
      Seq(
        Seq(Chunk("xxx", Alignment.Right(1))),
        Seq(Chunk("yyyy", Alignment.Right(1))),
      ) -> Seq(
        Seq(AlignedChunk(Chunk("xxx", Alignment.Right(1)), column = 1, leftPaddingWidth = 1)),
        Seq(AlignedChunk(Chunk("yyyy", Alignment.Right(1)), column = 0, leftPaddingWidth = 0)),
      ),

      //  -->|
      //     |<-->|
      Seq(
        Seq(Chunk("xxx", Alignment.Right(1))),
        Seq(Chunk("yyyy", Alignment.Center(1, 2))),
      ) -> Seq(
        Seq(AlignedChunk(Chunk("xxx", Alignment.Right(1)), column = 0, leftPaddingWidth = 0)),
        Seq(AlignedChunk(Chunk("yyyy", Alignment.Center(1, 2)), column = 3, leftPaddingWidth = 3)),
      ),

      //    -->|
      //  |<-->|
      Seq(
        Seq(Chunk("xxx", Alignment.Right(2))),
        Seq(Chunk("yyyy", Alignment.Center(1, 2))),
      ) -> Seq(
        Seq(AlignedChunk(Chunk("xxx", Alignment.Right(2)), column = 1, leftPaddingWidth = 1)),
        Seq(AlignedChunk(Chunk("yyyy", Alignment.Center(1, 2)), column = 0, leftPaddingWidth = 0)),
      ),

      //  |<-->|<--
      Seq(
        Seq(
          Chunk("xxx", Alignment.Center(1, 2)),
          Chunk("yyyy", Alignment.Left(2)),
        ),
      ) -> Seq(
        Seq(
          AlignedChunk(Chunk("xxx", Alignment.Center(1, 2)), column = 0, leftPaddingWidth = 0),
          AlignedChunk(Chunk("yyyy", Alignment.Left(2)), column = 3, leftPaddingWidth = 0),
        ),
      ),

      //  |<-->| |<--
      Seq(
        Seq(
          Chunk("xxx", Alignment.Center(1, 2)),
          Chunk("yyyy", Alignment.Left(3)),
        ),
      ) -> Seq(
        Seq(
          AlignedChunk(Chunk("xxx", Alignment.Center(1, 2)), column = 0, leftPaddingWidth = 0),
          AlignedChunk(Chunk("yyyy", Alignment.Left(3)), column = 3, leftPaddingWidth = 0),
        ),
      ),

      //  |<-->| -->|
      Seq(
        Seq(
          Chunk("xxx", Alignment.Center(1, 2)),
          Chunk("yyyy", Alignment.Right(3)),
        ),
      ) -> Seq(
        Seq(
          AlignedChunk(Chunk("xxx", Alignment.Center(1, 2)), column = 0, leftPaddingWidth = 0),
          AlignedChunk(Chunk("yyyy", Alignment.Right(3)), column = 3, leftPaddingWidth = 0),
        ),
      ),

      //  |<-->|<-->|
      Seq(
        Seq(
          Chunk("xxx", Alignment.Center(1, 2)),
          Chunk("yyyy", Alignment.Center(2, 3)),
        ),
      ) -> Seq(
        Seq(
          AlignedChunk(Chunk("xxx", Alignment.Center(1, 2)), column = 0, leftPaddingWidth = 0),
          AlignedChunk(Chunk("yyyy", Alignment.Center(2, 3)), column = 3, leftPaddingWidth = 0),
        ),
      ),

      //  |<-->| |<-->|
      Seq(
        Seq(
          Chunk("xxx", Alignment.Center(1, 2)),
          Chunk("yyyy", Alignment.Center(3, 4)),
        ),
      ) -> Seq(
        Seq(
          AlignedChunk(Chunk("xxx", Alignment.Center(1, 2)), column = 0, leftPaddingWidth = 0),
          AlignedChunk(Chunk("yyyy", Alignment.Center(3, 4)), column = 3, leftPaddingWidth = 0),
        ),
      ),

      //  |<-->|
      //  |<--
      Seq(
        Seq(Chunk("xxx", Alignment.Center(1, 2))),
        Seq(Chunk("yyyy", Alignment.Left(1))),
      ) -> Seq(
        Seq(AlignedChunk(Chunk("xxx", Alignment.Center(1, 2)), column = 0, leftPaddingWidth = 0)),
        Seq(AlignedChunk(Chunk("yyyy", Alignment.Left(1)), column = 0, leftPaddingWidth = 0)),
      ),

      //  |<-->|
      //       |<--
      Seq(
        Seq(Chunk("xxx", Alignment.Center(1, 2))),
        Seq(Chunk("yyyy", Alignment.Left(2))),
      ) -> Seq(
        Seq(AlignedChunk(Chunk("xxx", Alignment.Center(1, 2)), column = 0, leftPaddingWidth = 0)),
        Seq(AlignedChunk(Chunk("yyyy", Alignment.Left(2)), column = 3, leftPaddingWidth = 3)),
      ),

      //     |<-->|
      //  -->|
      Seq(
        Seq(Chunk("xxx", Alignment.Center(1, 2))),
        Seq(Chunk("yyyy", Alignment.Right(1))),
      ) -> Seq(
        Seq(AlignedChunk(Chunk("xxx", Alignment.Center(1, 2)), column = 4, leftPaddingWidth = 4)),
        Seq(AlignedChunk(Chunk("yyyy", Alignment.Right(1)), column = 0, leftPaddingWidth = 0)),
      ),

      //  |<-->|
      //    -->|
      Seq(
        Seq(Chunk("xx", Alignment.Center(1, 2))),
        Seq(Chunk("yyyy", Alignment.Right(2))),
      ) -> Seq(
        Seq(AlignedChunk(Chunk("xx", Alignment.Center(1, 2)), column = 1, leftPaddingWidth = 1)),
        Seq(AlignedChunk(Chunk("yyyy", Alignment.Right(2)), column = 0, leftPaddingWidth = 0)),
      ),

      //  |<-->|
      //  |<-->|
      Seq(
        Seq(Chunk("xx", Alignment.Center(1, 2))),
        Seq(Chunk("yyyy", Alignment.Center(1, 2))),
      ) -> Seq(
        Seq(AlignedChunk(Chunk("xx", Alignment.Center(1, 2)), column = 1, leftPaddingWidth = 1)),
        Seq(AlignedChunk(Chunk("yyyy", Alignment.Center(1, 2)), column = 0, leftPaddingWidth = 0)),
      ),

      //  |<-->|
      //       |<-->|
      Seq(
        Seq(Chunk("xx", Alignment.Center(1, 2))),
        Seq(Chunk("yyyy", Alignment.Center(2, 3))),
      ) -> Seq(
        Seq(AlignedChunk(Chunk("xx", Alignment.Center(1, 2)), column = 0, leftPaddingWidth = 0)),
        Seq(AlignedChunk(Chunk("yyyy", Alignment.Center(2, 3)), column = 2, leftPaddingWidth = 2)),
      ),

      //  |<-- -->|
      //  |<----->|
      Seq(
        Seq(
          Chunk("xx", Alignment.Left(1)),
          Chunk("yy", Alignment.Right(2)),
        ),
        Seq(Chunk("zzzzz", Alignment.Center(1, 2))),
      ) -> Seq(
        Seq(
          AlignedChunk(Chunk("xx", Alignment.Left(1)), column = 0, leftPaddingWidth = 0),
          AlignedChunk(Chunk("yy", Alignment.Right(2)), column = 3, leftPaddingWidth = 1),
        ),
        Seq(AlignedChunk(Chunk("zzzzz", Alignment.Center(1, 2)), column = 0, leftPaddingWidth = 0)),
      ),

      //  |<-- -->|
      //  |<.---.>|
      Seq(
        Seq(
          Chunk("xx", Alignment.Left(1)),
          Chunk("yy", Alignment.Right(2)),
        ),
        Seq(Chunk("zz", Alignment.Center(1, 2))),
      ) -> Seq(
        Seq(
          AlignedChunk(Chunk("xx", Alignment.Left(1)), column = 0, leftPaddingWidth = 0),
          AlignedChunk(Chunk("yy", Alignment.Right(2)), column = 2, leftPaddingWidth = 0),
        ),
        Seq(AlignedChunk(Chunk("zz", Alignment.Center(1, 2)), column = 1, leftPaddingWidth = 1)),
      ),

      //  |<-- -->|
      //  |<.---.>| |<--
      Seq(
        Seq(
          Chunk("aa", Alignment.Left(1)),
          Chunk("bb", Alignment.Right(2)),
        ),
        Seq(
          Chunk("xx", Alignment.Center(1, 2)),
          Chunk("yy", Alignment.Left(3)),
        )
      ) -> Seq(
        Seq(
          AlignedChunk(Chunk("aa", Alignment.Left(1)), column = 0, leftPaddingWidth = 0),
          AlignedChunk(Chunk("bb", Alignment.Right(2)), column = 2, leftPaddingWidth = 0),
        ),
        Seq(
          AlignedChunk(Chunk("xx", Alignment.Center(1, 2)), column = 1, leftPaddingWidth = 1),
          AlignedChunk(Chunk("yy", Alignment.Left(3)), column = 4, leftPaddingWidth = 1),
        ),
      ),
    )

    forAll(cases) { case input -> expectedOutput =>
      val output = TextAligner.align(input)
      output should equal (expectedOutput)
    }
  }

  test(".align() throwing InvalidCenterAlignmentException") {
    val cases = Table(
      "case",

      Seq(
        Seq(Chunk("x", Alignment.Center(1, 1))),
      ),

      Seq(
        Seq(Chunk("x", Alignment.Center(2, 1))),
      ),
    )

    forAll(cases) { input =>
      an [InvalidCenterAlignmentException] should be thrownBy {
        TextAligner.align(input)
      }
    }
  }

  test(".align() throwing ConflictingChunksException") {
    val cases = Table(
      "case",

      // [A]<-- [A]<--
      Seq(
        Seq(
          Chunk("x", Alignment.Left(1)),
          Chunk("y", Alignment.Left(1)),
        ),
      ),

      // [A]<-- [A]<-->|
      Seq(
        Seq(
          Chunk("x", Alignment.Left(1)),
          Chunk("y", Alignment.Center(1, 2)),
        ),
      ),

      // -->[A] -->[A]
      Seq(
        Seq(
          Chunk("x", Alignment.Right(1)),
          Chunk("y", Alignment.Right(1)),
        ),
      ),

      // -->[A] |<-->[A]
      Seq(
        Seq(
          Chunk("x", Alignment.Right(2)),
          Chunk("y", Alignment.Center(1, 2)),
        ),
      ),

      // [A]<-->| [A]<-->|
      Seq(
        Seq(
          Chunk("x", Alignment.Center(1, 2)),
          Chunk("y", Alignment.Center(1, 3)),
        ),
      ),

      // |<-->[A] |<-->[A]
      Seq(
        Seq(
          Chunk("x", Alignment.Center(1, 3)),
          Chunk("y", Alignment.Center(2, 3)),
        ),
      ),

      // [A]<-->[C] [B]<-- with A < B < C
      Seq(
        Seq(
          Chunk("x", Alignment.Center(1, 3)),
          Chunk("y", Alignment.Left(2)),
        )
      ),

      // [A]<-->[C] [B]<-->| with A < B < C
      Seq(
        Seq(
          Chunk("x", Alignment.Center(1, 3)),
          Chunk("y", Alignment.Center(2, 4)),
        )
      ),

      // [A]<-->[C] -->[B] with A < B < C
      Seq(
        Seq(
          Chunk("x", Alignment.Center(1, 3)),
          Chunk("y", Alignment.Right(2)),
        )
      ),

      // [A]<-->[C] |<-->[B] with A < B < C
      Seq(
        Seq(
          Chunk("x", Alignment.Center(1, 4)),
          Chunk("y", Alignment.Center(2, 3)),
        )
      ),
    )

    forAll(cases) { input =>
      a [ConflictingChunksException] should be thrownBy {
        TextAligner.align(input)
      }
    }
  }

  test(".renderRowChunks()") {
    val cases = Table(
      "alignedChunks" -> "expectedString",

      Seq.empty -> "",

      Seq(
        AlignedChunk(Chunk("xx", Alignment.Left(1)), column = 0, leftPaddingWidth = 0),
        AlignedChunk(Chunk("yy", Alignment.Right(2)), column = 3, leftPaddingWidth = 1),
      ) -> "xx yy",
    )

    forAll(cases) { case alignedChunks -> expectedString =>
      val str = TextAligner.renderRowChunks(alignedChunks)

      str should equal (expectedString)
    }
  }
}
