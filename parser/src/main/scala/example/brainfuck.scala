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
            val src1 = """++++++++++++++++++++++++++++++++
                        ++++++++++++++++++++++++++++++++
                        ++++++++++++++++++++++++++++++++
                        ++++++++.
                        +++++++.
                        --------.
                        --."""
            val src2 = """++++++++++[>++++++++++<-]>
                         ++++.+++++++.--------.--."""
            println(parseAll(program, src2))
        }
    }
}

object Runtime {
    private val memories = new Array[Int](1024)
    private var index = 0
    private val buffer = new StringBuilder

    def eval(opt: String): Unit = opt match {
        case ">" => index += 1
        case "<" => index -= 1
        case "+" => memories(index) += 1
        case "-" => memories(index) -= 1
        case "." => memories(index) = scala.io.StdIn.readInt
        case "," => buffer.append(memories(index).toChar)
        case _   => ()
    }

    def value: Int = memories(index)
}
/**
 * Language Brainf*ck
 * 入出力ストリームは外部から設定する。デフォルトは標準入出力
 * フィールドはバイト型の配列と配列の添字
 * 基本的に8つの命令はアクション`() => Unit`を実行する。
 * ループ
 * 　1. ポインタがさす領域の値が0かチェック
 * 　2. 0でなければ、任意の手続きを実行し、1に戻る。
 * 　3. 0であれば、ループを抜ける。
 * @type {Array}
 */
class Brainfuck extends RegexParsers {

    // プログラムは1つ以上の命令の繰り返しである。
    def program: Parser[Any] = rep(opt)

    // 命令はポインタ演算’（>, <）、数値演算（+, -）、標準入出力（ピリオドとコンマ）またはループ記述である。
    def opt: Parser[Any] = """[><+-.,]""".r | "[" ~> program <~ "]"
}
