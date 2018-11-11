

object Main extends App {
    val obj = new Main
    obj.start
}

/**
 * 三目ならべ
 */
class Main {

    private val PLAYER01 = '○'
    private val PLAYER02 = '×'

    private val board = Array.fill(9)(new Mass())

    // 横のマス
    private val holizontal = List(
        List(board(0), board(1), board(2)),
        List(board(3), board(4), board(5)),
        List(board(6), board(7), board(8))
    )

    // 縦のマス
    private val vertical = List(
        List(board(0), board(3), board(6)),
        List(board(1), board(4), board(7)),
        List(board(2), board(5), board(8))
    )

    //斜めのマス
    private val diagonal = List(
        List(board(0), board(4), board(8)),
        List(board(2), board(4), board(6))
    )

    /**
     * 勝利判定をする。
     * 横方向に同じマークが並んでいる。または、縦方向に同じマークが並んでいる。または、ななめ方向に同じマークが並んでいる。
     * @param mark 判定するマーク
     * @return 勝利の場合、true, そうでなければ、false
     */
    private val decision : (Char => Boolean) = mark => {
        val directions = List(holizontal, vertical, diagonal)

        // 判定のロジック
        val functionLogic = (direction: List[List[Mass]]) => {
            val marks = direction.map(xs => xs.map(x => x.mark))
            val isParamMarks = marks.map(xs => (true /: xs)((acc, mass) => acc && (mass == mark)))
            (false /: isParamMarks)((mass, acc) => mass || acc)
        }

        // 縦、横、斜めの中で引数のマークが並んでいる場合、true
        (false /: directions.map(functionLogic))((acc, isParamMark) => acc || isParamMark)
    }

    /**
     * 標準入力から盤面の位置（0~9）を取得し、任意のマスに引数のマークを付与する。
     */
    private val put : (Char => Unit) = mark => {
        val enteredString = scala.io.StdIn.readLine
        if(enteredString == "q") {
            scala.sys.exit(1)
        } else {
            val index = enteredString.toInt
            if (board(index).mark == '-') {
                board(index).mark = mark
            } else {
                println("Already entered!!")
                put(mark)
            }
        }
    }

    private val changePlayer : (Char => Char) = value => value match {
        case PLAYER01 => PLAYER02
        case PLAYER02 => PLAYER01
        case _        => throw new Exception("Undefined Player!")
    }


    private val showBoard : (() => Unit) = () => {
        val row1 = board.slice(0, 3)
        val row2 = board.slice(3, 6)
        val row3 = board.slice(6, 9)
        for(row <- List(row1, row2, row3)){
            for(mass <- row) print(mass.mark + " ")
            print("\n")
        }
    }

    /**
     * 三目並べのルーチン
     * @param player ○か×の文字
     * @return ゲーム終了時のメッセージ
     */

    private val gameRoutine : (Char => String) = player => {
        val markList = for(mass <- board) yield mass.mark
        val boardIsFull = (true /: markList)((acc, mark) => (mark != '-') && acc)
        if (!boardIsFull) {
            put(player)
            showBoard()
            if (decision(player)) {
                player.toString + " is win!!"
            }else{
                val nextRoutine = gameRoutine compose changePlayer
                nextRoutine(player)
            }
        } else {
            "drow..."
        }

    }

    def start(): Unit = {
        println("Game Start!")
        val message = gameRoutine('○')
        println(message)
    }
}

class Mass(aMark: Char) {
    var mark = if(aMark == '○' || aMark == '×') aMark else '-'
    def this() = this('-')
}
