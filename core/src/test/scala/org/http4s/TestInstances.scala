package org.http4s

import java.nio.charset.{Charset => NioCharset}
import java.time._
import java.time.temporal.ChronoUnit

import scala.collection.JavaConverters._
import scala.collection.immutable.BitSet

import cats.data.NonEmptyList
import org.http4s.batteries._
import org.http4s.headers._
import org.http4s.util.string._

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.{ Arbitrary, Gen }

trait TestInstances {
  implicit class ParseResultSyntax[A](self: ParseResult[A]) {
    def yolo: A = self.valueOr(e => sys.error(e.toString))
  }

  implicit def NonEmptyListArbitrary[A: Arbitrary]: Arbitrary[NonEmptyList[A]] =
    Arbitrary { for {
      a <- arbitrary[A]
      list <- arbitrary[List[A]]
    } yield NonEmptyList(a, list) }

  lazy val tchars: Gen[Char] = oneOf {
    Seq('!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '^', '_', '`', '|', '~') ++
      ('0' to '9') ++ ('A' to 'Z') ++ ('a' to 'z')
  }
  lazy val tokens: Gen[String] =
    nonEmptyListOf(tchars).map(_.mkString)

  lazy val fieldValues: Gen[String] =
    fieldContents

  lazy val fieldContents: Gen[String] =
    for {
      head <- fieldVchars
      tail <- containerOf[Vector, Vector[Char]](
        frequency(
          9 -> fieldVchars.map(Vector(_)),
          1 -> (for {
            spaces <- nonEmptyContainerOf[Vector, Char](oneOf(' ', '\t'))
            fieldVchar <- fieldVchars
          } yield spaces :+ fieldVchar)
        )
      ).map(_.flatten)
    } yield (head +: tail).mkString

  lazy val fieldVchars: Gen[Char] =
    vchars

  lazy val vchars: Gen[Char] =
    oneOf('\u0021' to '\u007e')

  lazy val standardMethods: Gen[Method] =
    oneOf(Method.registered.toSeq)

  implicit lazy val arbitraryMethod: Arbitrary[Method] = Arbitrary(frequency(
    10 -> standardMethods,
    1 -> tokens.map(Method.fromString(_).yolo)
  ))

  lazy val validStatusCodes =
    choose(100, 599)

  lazy val standardStatuses =
    oneOf(Status.registered.toSeq)

  lazy val customStatuses = for {
    code <- validStatusCodes
    reason <- arbString.arbitrary
  } yield Status.fromIntAndReason(code, reason).yolo
  implicit lazy val arbitraryStatus: Arbitrary[Status] = Arbitrary(frequency(
    10 -> standardStatuses,
    1 -> customStatuses
  ))

  implicit lazy val arbitraryQueryParam: Arbitrary[(String, Option[String])] =
    Arbitrary { frequency(
      5 -> { for {
                k <- arbitrary[String]
                v <- arbitrary[Option[String]]
              } yield (k, v)
           },
      2 -> const(("foo" -> Some("bar")))  // Want some repeats
    ) }

  implicit lazy val arbitraryQuery: Arbitrary[Query] =
    Arbitrary { for {
      n <- size
      vs <- containerOfN[Vector, (String, Option[String])](n % 8, arbitraryQueryParam.arbitrary)
    } yield Query(vs:_*) }

  implicit lazy val arbitraryHttpVersion: Arbitrary[HttpVersion] =
    Arbitrary { for {
      major <- choose(0, 9)
      minor <- choose(0, 9)
    } yield HttpVersion.fromVersion(major, minor).yolo }

  implicit lazy val arbitraryNioCharset: Arbitrary[NioCharset] =
    Arbitrary(oneOf(NioCharset.availableCharsets.values.asScala.toSeq))

  implicit lazy val arbitraryCharset: Arbitrary[Charset] =
    Arbitrary { arbitrary[NioCharset].map(Charset.fromNioCharset) }

  implicit lazy val arbitraryQValue: Arbitrary[QValue] =
    Arbitrary { oneOf(const(0), const(1000), choose(0, 1000)).map(QValue.fromThousandths(_).yolo) }

  implicit lazy val arbitraryCharsetRange: Arbitrary[CharsetRange] =
    Arbitrary { for {
      charsetRange <- charsetRangesNoQuality
      q <- arbitrary[QValue]
    } yield charsetRange.withQValue(q) }

  implicit lazy val arbitraryCharsetAtomRange: Arbitrary[CharsetRange.Atom] =
    Arbitrary { for {
      charset <- arbitrary[Charset]
      q <- arbitrary[QValue]
    } yield charset.withQuality(q) }

  implicit lazy val arbitraryCharsetSplatRange: Arbitrary[CharsetRange.`*`] =
    Arbitrary { arbitrary[QValue].map(CharsetRange.`*`.withQValue(_)) }

  lazy val charsetRangesNoQuality: Gen[CharsetRange] =
    frequency(
      3 -> arbitrary[Charset].map(CharsetRange.fromCharset),
      1 -> const(CharsetRange.`*`)
    )

  implicit lazy val arbitraryAcceptCharset: Arbitrary[`Accept-Charset`] =
    Arbitrary { for {
      // make a set first so we don't have contradictory q-values
      charsetRanges <- nonEmptyContainerOf[Set, CharsetRange](charsetRangesNoQuality).map(_.toVector)
      qValues <- containerOfN[Vector, QValue](charsetRanges.size, arbitraryQValue.arbitrary)
      charsetRangesWithQ = charsetRanges.zip(qValues).map { case (range, q) => range.withQValue(q) }
    } yield `Accept-Charset`(charsetRangesWithQ.head, charsetRangesWithQ.tail:_*) }

  implicit lazy val arbitraryUrlForm: Arbitrary[UrlForm] = Arbitrary {
    // new String("\ufffe".getBytes("UTF-16"), "UTF-16") != "\ufffe".
    // Ain't nobody got time for that.
    arbitrary[Map[String, Seq[String]]].map(UrlForm.apply)
      .suchThat(!_.toString.contains('\ufffe'))
  }
 
  implicit lazy val arbitraryBitSet: Arbitrary[BitSet] = Arbitrary(
    Arbitrary.arbitrary[Set[Char]].map(_.map(_.toInt)).map(set => BitSet(set.toSeq: _*))
  )

  implicit lazy val arbitraryAllow: Arbitrary[Allow] =
    Arbitrary { for {
      methods <- nonEmptyContainerOf[Set, Method](arbitrary[Method]).map(_.toList)
    } yield Allow(methods.head, methods.tail:_*) }

  implicit lazy val arbitraryContentLength: Arbitrary[`Content-Length`] =
    Arbitrary { for {
      long <- arbitrary[Long] if long > 0L
    } yield `Content-Length`(long) }

  implicit lazy val arbitraryXB3TraceId: Arbitrary[`X-B3-TraceId`] =
    Arbitrary { for {
      long <- arbitrary[Long]
    } yield `X-B3-TraceId`(long) }

  implicit lazy val arbitraryXB3SpanId: Arbitrary[`X-B3-SpanId`] =
    Arbitrary { for {
      long <- arbitrary[Long]
    } yield `X-B3-SpanId`(long) }

  implicit lazy val arbitraryXB3ParentSpanId: Arbitrary[`X-B3-ParentSpanId`] =
    Arbitrary { for {
      long <- arbitrary[Long]
    } yield `X-B3-ParentSpanId`(long) }

  implicit lazy val arbitraryXB3Flags: Arbitrary[`X-B3-Flags`] =
    Arbitrary { for {
      flags <- Gen.listOfN(3, Gen.oneOf(
        `X-B3-Flags`.Flag.Debug,
        `X-B3-Flags`.Flag.Sampled,
        `X-B3-Flags`.Flag.SamplingSet))
    } yield `X-B3-Flags`(flags.toSet) }

  implicit lazy val arbitraryXB3Sampled: Arbitrary[`X-B3-Sampled`] =
    Arbitrary { for {
      boolean <- arbitrary[Boolean]
    } yield `X-B3-Sampled`(boolean) }

  lazy val httpDateInstant: Gen[Instant] = {
    // RFC 5322 says 1900 is the minimum year
    val min = ZonedDateTime.of(1900, 1, 1, 0, 0, 0, 0, ZoneId.of("UTC")).toInstant.toEpochMilli
    val max = ZonedDateTime.of(9999, 12, 31, 23, 59, 59, 0, ZoneId.of("UTC")).toInstant.toEpochMilli
    choose[Long](min, max).map(Instant.ofEpochMilli(_).truncatedTo(ChronoUnit.SECONDS))
  }

  implicit lazy val arbitraryDateHeader: Arbitrary[headers.Date] =
    Arbitrary { for {
      instant <- httpDateInstant
    } yield headers.Date(instant) }

  lazy val httpExpireInstant: Gen[Instant] = {
    // RFC 2616 says Expires should be between now and 1 year in the future, though other values are allowed
    val min = ZonedDateTime.of(LocalDateTime.now, ZoneId.of("UTC")).toInstant.toEpochMilli
    val max = ZonedDateTime.of(LocalDateTime.now.plusYears(1), ZoneId.of("UTC")).toInstant.toEpochMilli
    choose[Long](min, max).map(Instant.ofEpochMilli(_).truncatedTo(ChronoUnit.SECONDS))
  }

  implicit lazy val arbitraryExpiresHeader: Arbitrary[headers.Expires] =
    Arbitrary { for {
      instant <- httpExpireInstant
    } yield headers.Expires(instant) }

  implicit lazy val arbitraryRawHeader: Arbitrary[Header.Raw] =
    Arbitrary {
      for {
        token <- tokens
        value <- fieldValues
      } yield Header(token, value)
    }

  implicit lazy val arbitraryHeader: Arbitrary[Header] =
    Arbitrary {
      oneOf(
        arbitrary[`Accept-Charset`],
        arbitrary[Allow],
        arbitrary[`Content-Length`],
        arbitrary[Date],
        arbitrary[Header.Raw]
      )
    }

  /* TODO fs2 port
  implicit lazy val arbitraryServerSentEvent: Arbitrary[ServerSentEvent] = {
    import ServerSentEvent._
    def singleLineString: Gen[String] =
      arbitrary[String] suchThat { s => !s.contains("\r") && !s.contains("\n") }
    Arbitrary(for {
      data <- singleLineString
      event <- frequency(
        4 -> None,
        1 -> singleLineString.map(Some.apply)
      )
      id <- frequency(
        8 -> None,
        1 -> Some(EventId.reset),
        1 -> (singleLineString suchThat (_.nonEmpty)).map(id => Some(EventId(id)))
      )
      retry <- frequency(
        4 -> None,
        1 -> posNum[Long].map(Some.apply)
      )
    } yield ServerSentEvent(data, event, id, retry))
  }
   */
}
