package example

import java.io.InputStream
import java.io.BufferedInputStream
import java.io.OutputStream
import java.io.BufferedOutputStream
import scala.util.parsing.combinator._


object Parser extends Brainfuck {
    def main(args: Array[String]) {
        if (args.length == 2) {

        } else {
            val src1 = """
                ++++++++++++++++++++++++++++++++
                ++++++++++++++++++++++++++++++++
                ++++++++++++++++++++++++++++++++
                ++++++++.
                +++++++.
                --------.
                --.
            """
            val src2 = """
                ++++++++++[>++++++++++<-]>
                ++++.+++++++.--------.--.
            """
            val src3 = """
                >+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++
                ++>-]<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>
                ++++++++[<++++>-]<+.[-]++++++++++.
            """

            val src4 = "-"
            // println(parseAll(program, src3))
            apply(src3)
        }
    }
}


object Runtime {
    private val memories = new Array[Int](1024)
    private var index = 0
    private val buffer = new StringBuilder

    abstract class RuntimeObject{
        def evaluate: Unit
    }
    class Prog(procs: List[Proc]) extends RuntimeObject {
        override def evaluate: Unit = {
            println("*********** Evaluating Brainf*ck START **********")
            procs.map(_.evaluate)
            println("*********** Evaluating Brainf*ck END   **********")
        }
    }

    abstract class Proc extends RuntimeObject
    class Opt(tok: String) extends Proc {
        override def evaluate: Unit = tok match {
            case ">" => index += 1
            case "<" => index -= 1
            case "+" => memories(index) += 1
            case "-" => memories(index) -= 1
            case "." => print(memories(index).toChar)
            case "," => memories(index) = scala.io.StdIn.readInt
            case _   => ()
        }

    }
    class Loop(toks: List[Proc]) extends Proc {
        override def evaluate: Unit = while(memories(index) != 0) toks.map(_.evaluate)
    }
}

/**
 * Language Brainf*ck
 */
class Brainfuck extends RegexParsers {

    def apply(input: String): Unit = parseAll(program, input) match {
        case Success(result, _) => result.evaluate
        case failure: NoSuccess => scala.sys.error(failure.msg)
    }

    def program  : Parser[Runtime.Prog] = rep(procedure) ^^ (new Runtime.Prog(_))

    def procedure: Parser[Runtime.Proc] = operator | loop

    def operator : Parser[Runtime.Opt] = """[><+-.,]""".r ^^ (new Runtime.Opt(_))
    def loop     : Parser[Runtime.Loop] = "[" ~> rep(procedure) <~ "]" ^^ (new Runtime.Loop(_))
}
