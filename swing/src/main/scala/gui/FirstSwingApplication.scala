package gui

import scala.swing._
import scala.swing.event._

object SwingApplications extends SimpleSwingApplication {
    private val apps = new SwingApplications
    def top = new MainFrame {
        title = "Swing Applications"
        object button1 extends Button { text = "First Swing Application" }
        object button2 extends Button { text = "Temp Converter" }
        object Header extends FlowPanel {
            contents += button1
            contents += button2
        }
        contents = Header
        listenTo(button1, button2)

        reactions += {
            case ButtonClicked(`button1`) =>
                contents = new BoxPanel(Orientation.Vertical) {
                    contents += Header
                    contents += new Label { text = "First Swing Application" }
                    contents += apps.pattern1
                }
            case ButtonClicked(`button2`) =>
                contents = new BoxPanel(Orientation.Vertical) {
                    contents += Header
                    contents += new Label { text = "Temp Converter" }
                    contents += apps.pattern2
                }
        }
    }
}

class SwingApplications {

    private val pattern1: BoxPanel = new BoxPanel(Orientation.Vertical) {
        object button extends Button { text = "Click me" }
        contents += button
    }
    /**
     * 摂氏・華氏換算プログラム
     * @return Frame
     */
    private val pattern2: FlowPanel = new FlowPanel {
        object celsius extends TextField { columns = 5 }
        object fahrenheit extends TextField { columns = 5 }
        contents += new Label(" Celsius = ")
        contents += celsius
        contents += new Label(" Fahrenheit = ")
        contents += fahrenheit
        border = Swing.EmptyBorder(15, 10, 10, 10)
        listenTo(celsius, fahrenheit)
        reactions += {
            case EditDone(`fahrenheit`) =>
                val f = fahrenheit.text.toInt
                val c = (f - 32) * 5 / 9
                celsius.text = c.toString
            case EditDone(`celsius`) =>
                val c = celsius.text.toInt
                val f = c * 9 / 5 + 32
                fahrenheit.text = f.toString
        }
    }
}
