package com.codelab27.cards9.models

import com.codelab27.cards9.models.ModelGens._
import com.codelab27.cards9.models.cards.Arrow
import com.codelab27.cards9.specs.ModelSpec

class ArrowSpec extends ModelSpec {
  val zeroByte: Byte = 0x00
  val maxByte: Byte = 0xFF.toByte

  "A packed arrows" when {
    "zero" should {
      "return empty list of arrows" in {
        Arrow.extract(zeroByte) shouldBe Nil
      }
    }

    "all bits set to one" should {
      "return list of arrows of size MAX_ARROWS" in {
        Arrow.extract(maxByte).size shouldEqual Arrow.MAX_ARROWS
      }

      "return list of arrows composed of all arrows" in {
        Arrow.extract(maxByte).toSet shouldEqual Arrow.values.toSet
      }
    }

    "random bits are set to one" should {
      "compressed arrows should give the same packed byte" in {
        forAll { (packed: Byte) =>
          Arrow.compress(Arrow.extract(packed)) shouldBe Some(packed)
        }
      }
    }
  }

  "A list of arrows" when {
    "empty" should {
      "return a zero compressed byte" in {
        Arrow.compress(Nil) shouldEqual Some(zeroByte)
      }
    }

    "arrows are repeated and/or size is greater than MAX_ARROWS" should {
      "return no byte" in {
        forAll(invalidArrowsGenerator) { arrows: List[Arrow] =>
          whenever(arrows.distinct.size != arrows.size || arrows.size > Arrow.MAX_ARROWS) {
            Arrow.compress(arrows) shouldEqual None
          }
        }
      }
    }

    "valid random arrows are selected" should {
      "compress and extract the same list" in {
        forAll { arrows: List[Arrow] =>
          Arrow.compress(arrows).map(Arrow.extract(_).toSet) shouldBe Some(arrows.toSet)
        }
      }
    }

  }

}
