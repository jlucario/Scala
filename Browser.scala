import swing._
import scala.collection.mutable._
import java.awt.BorderLayout
import javax.swing.JEditorPane
import java.awt.ScrollPane
import javax.swing.JScrollPane
import javax.swing.JOptionPane
import java.io.InputStreamReader
import javax.swing.event.HyperlinkListener
import javax.swing.event.HyperlinkEvent
import java.io.BufferedReader
import java.net.URL
import scala.swing.TextField
import java.net.MalformedURLException

object Browser extends SimpleSwingApplication {
  // Lists declare
  var book = Map("hello" -> "test")

  var forwardStack = Stack("sad")

  //clear lists
  forwardStack.clear()
  book.clear()

  def top = new MainFrame {
    // Main html viewer area 
    val htmlPane = new JEditorPane("text/html", "")
    htmlPane.setEditable(false)
  
 

    // Address bar
    var AddressBar = new TextField {
      preferredSize = new Dimension(7,7)
    }

    val lisner = new Listener(htmlPane, AddressBar)
    htmlPane.addHyperlinkListener(lisner)

    val scrol = new JScrollPane(htmlPane)
    val wrapHtmlPane = Component.wrap(scrol)

    
    // Home address
    var Home = "http://pers-www.wlv.ac.uk/~in0316/test2.html"
    title = "James's Browser"
    visible = true
    preferredSize = new Dimension(1000, 1000)

    // Back
    val Back = new Button {
       
      action = Action("Previous") {
        if (!lisner.backStack.isEmpty) {
          var url = lisner.backStack.pop()
          forwardStack.push(url)
          htmlPane.setText(lisner.readText(url))
        }
      }
    }
    // Forward 
    val Forward = new Button {
      action = Action("Forward") {
        if (!forwardStack.isEmpty) {
          var url = forwardStack.pop()
          lisner.backStack.push(url)
          htmlPane.setText(lisner.readText(url))
        }
      }
    }
    // Reload 
    val Reload = new Button {
      action = Action("Reload") {
        var url = AddressBar.text
        if (!(url == ""))
          htmlPane.setText(lisner.readText(url))
      }
    }
    // Home
    val home = new Button {

      action = Action("Home Page") {
        AddressBar.text = Home
        lisner.backStack.push(Home)
        htmlPane.setText(lisner.readText(Home))
      }
    }

    //  Go
    val Go = new Button() {
      action = Action("Go") {
        var url = AddressBar.text
        lisner.backStack.push(url)
        htmlPane.setText(lisner.readText(url))
      }
    }

   
    
    //grid layout
    var gridMenu = new GridPanel(1, 5) {
      contents += Back
      contents += Forward
      contents += Reload
      contents += Go
      contents += home
      
    }
    
     // grid layout address
    var addPnl = new GridPanel(2,1){
      contents += AddressBar
      contents +=gridMenu

    }
    // Menu bar

    val mnubar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem("Bookmark Current Page") { action = Action("Bookmark Current Page") { bookmark(AddressBar.text) } }
        contents += new MenuItem("List Bookmarks") { action = Action("List Bookmarks") { JOptionPane.showMessageDialog(null,book)} }
        contents += new MenuItem("Exit") { action = Action("Exit") { exit() } }
      }
      contents += new Menu("Edit") {
        contents += new MenuItem("Set as Home"){action = Action("Set as Home"){Home = AddressBar.text}}
        contents += new MenuItem("About"){action = Action("About"){JOptionPane.showMessageDialog(null,
	"James Braznell \n 1007022")}}

      }

    }

    // Container to hold menu and buttons
    val topContent = new BorderPanel {
      add(mnubar,BorderPanel.Position.North)
      add(addPnl,BorderPanel.Position.South)
    }
    // Main Layout

    contents = new BorderPanel {
      add(topContent, BorderPanel.Position.North);
      add(wrapHtmlPane, BorderPanel.Position.Center)
    }

  }

  // Bookmark any web link
  def bookmark(address: String): Unit = {
   

    if (!(address == "")) {
      var msg = JOptionPane.showInputDialog(null, "Web Address : \n" + address + "\n Type name", "Bookmark", JOptionPane.PLAIN_MESSAGE)
      if (msg == "")
        JOptionPane.showMessageDialog(null, "Please type name of bookmark", "Error", JOptionPane.ERROR_MESSAGE)
      else
        book = book + (msg -> address)

    }
  }
}




class Listener(editorPane : JEditorPane, address : TextField) extends HyperlinkListener {
  var backStack = Stack("sda")
   backStack.clear()

	def hyperlinkUpdate(hyperlinkEvent : HyperlinkEvent ) {
		val type1 = hyperlinkEvent.getEventType();
		var url = hyperlinkEvent.getURL();
		var url1: String = url.toString()
		
		if (type1 == HyperlinkEvent.EventType.ACTIVATED) {
			editorPane.setText(readText(url1))	
			backStack.push(url1)
			address.text=url1
		}
	}
	 // Read text from websites
  def readText(urlText: String): String = {
    var result = ""
      if(!(urlText == null || urlText =="")){
        try{
    val url = new URL(urlText)
    val connection = url.openConnection
    val reader = new BufferedReader(
      new InputStreamReader(connection.getInputStream()))
    var line = reader.readLine
    while (line != null) {
      result = result + line
      line = reader.readLine
    }
    reader.close();
        }catch{
          case ec : MalformedURLException => ( JOptionPane.showMessageDialog(null,"Wrong url.","Error",JOptionPane.ERROR_MESSAGE))
          case file : java.io.FileNotFoundException=>(JOptionPane.showMessageDialog(null,"Page not found.","Error",JOptionPane.ERROR_MESSAGE))
          case ex : Exception => (println(ex))
         
          
        }
      }else
        JOptionPane.showMessageDialog(null,"Type url.","Error",JOptionPane.ERROR_MESSAGE)
    result
  }
  
}


