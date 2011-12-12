import com.biosimilarity.lift.lib.node._
val jrrl1 = new JSONRequestREPL()
jrrl1.exchangeNode( "localhost", "localhost", "node-direct-trgt-exchange", "node-direct-src-exchange")