

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

    private val board = Array(
        new Mass(), new Mass(), new Mass(),
        new Mass(), new Mass(), new Mass(),
        new Mass(), new Mass(), new Mass(),
    )

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
    private def decision(mark: Char): Boolean = {
        val toMark = (xss: List[List[Mass]]) => xss.map((xs: List[Mass]) => xs.map(x => x.mark))
        val charToBoolean = (xss: List[List[Char]]) => xss.map((xs: List[Char]) => xs.foldLeft(true)((acc, mass) => acc && (mass == mark)))
        val res1 = charToBoolean(toMark(holizontal)).foldLeft(false)((mass, acc) => mass || acc)
        val res2 = charToBoolean(toMark(vertical)).foldLeft(false)((mass, acc) => mass || acc)
        val res3 = charToBoolean(toMark(diagonal)).foldLeft(false)((mass, acc) => mass || acc)
        return res1 || res2 || res3
    }

    /**
     * (x, y)のところに盤面に石を置く。
     * すでにコマが置かれている場合を考慮していないから考慮する必要がある。
     * @param x X座標
     * @param y Y座標
     */
    private def put(mark: Char): Unit = {
        val enteredString = scala.io.StdIn.readLine
        if(enteredString == "q")
            scala.sys.exit(1)
        else {
            val index = enteredString.toInt
            if(board(index).mark == '-')
                board(index).mark = mark
            else{
                println("Already entered!!")
                put(mark)
            }
        }
    }

    private def changePlayer(value: Char): Char = value match {
            case PLAYER01 => PLAYER02
            case PLAYER02 => PLAYER01
            case _ => throw new Exception("Undefined Player!")
    }

    private def showBoard(): Unit = {
        val row1 = List(board(0), board(1), board(2))
        val row2 = List(board(3), board(4), board(5))
        val row3 = List(board(6), board(7), board(8))
        for(row <- List(row1, row2, row3)){
            for(mass <- row){
                print(mass.mark + " ")
            }
            print("\n")
        }
    }

    /**
     * 三目並べのルーチン
     * @param player ○か×の文字
     * @return ゲーム終了時のメッセージ
     */
    private def gameRutine(player: Char): String = {
        val markList = board.map(mass => mass.mark)
        val boardIsFull = markList.foldLeft(true)((acc, mark) => (mark != '-') && acc)
        if(!boardIsFull){
            put(player)
            showBoard()
            if(decision(player))
                player.toString + " is win!!"
            else
                gameRutine(changePlayer(player))
        } else {
            "drow..."
        }

    }

    def start(): Unit = {
        println("Game Start!")
        val message = gameRutine('○')
        println(message)
    }
}

class Mass(aMark: Char) {
    var mark = if(aMark == '○' || aMark == '×') aMark else '-'
    def this() = this('-')
}
