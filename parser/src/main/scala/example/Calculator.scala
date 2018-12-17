package example

import scala.util.parsing.combinator._

object Calculator extends JavaTokenParsers {
    def apply(input: String): Double = parseAll(expr, input) match {
        case Success(result, _) => result
        case failure: NoSuccess => scala.sys.error(failure.msg)
    }
    def expr: Parser[Double] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
        case number ~ list => (number /: list) {
            case (x, "+" ~ y) => x + y
            case (x, "-" ~ y) => x - y
        }
    }
    def term: Parser[Double] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ {
        case number ~ list => (number /: list) {
            case (x, "*" ~ y) => x * y
            case (x, "/" ~ y) => x / y
        }
    }
    def factor: Parser[Double] = number | "(" ~> expr <~ ")"
    def number: Parser[Double] = floatingPointNumber ^^ { _.toDouble }
}
