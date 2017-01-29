package com.codelab27.cards9.models

import ModelGens._
import com.codelab27.cards9.models.cards.Arrow
import com.codelab27.cards9.specs.ModelSpec

class ArrowSpec extends ModelSpec {
  val zeroByte: Byte = 0x00
  val maxByte: Byte = 0xFF.toByte

  "A packed arrows" when {
    "zero" should {
      "return empty list of arrows" in {
        Arrow.extract(zeroByte) should be(Nil)
      }
    }

    "all bits set to one" should {
      "return list of arrows of size MAX_ARROWS" in {
        Arrow.extract(maxByte).size should be equals Arrow.MAX_ARROWS
      }

      "return list of arrows composed of all arrows" in {
        Arrow.extract(maxByte).toSet should be equals Arrow.values.toSet
      }
    }

    "random bits are set to one" should {
      "compressed arrows should give the same packed byte" in {
        forAll { (packed: Byte) =>
          Arrow.compress(Arrow.extract(packed)) should be(packed)
        }
      }
    }
  }

  "A list of arrows" when {
    "empty" should {
      "return a zero compressed byte" in {
        Arrow.compress(Nil) should be equals zeroByte
      }
    }

    "arrows are repeated and/or size is greater than MAX_ARROWS" should {
      "throw an IllegalArgumentException" in {
        forAll(InvalidArrowsGenerator) { arrows: List[Arrow] =>
          whenever(arrows.distinct.size != arrows.size || arrows.size > Arrow.MAX_ARROWS) {
            intercept[IllegalArgumentException] {
              Arrow.compress(arrows)
            }
          }
        }
      }
    }

    "valid random arrows are selected" should {
      "compress and extract the same list" in {
        forAll { arrows: List[Arrow] =>
          Arrow.extract(Arrow.compress(arrows)).toSet should be(arrows.toSet)
        }
      }
    }

  }

}
