// -*- mode: Scala;-*- 
// Filename:    Scribble.scala 
// Authors:     lgm                                                    
// Creation:    Mon Aug 19 20:32:53 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store.scribble
import scala.util.continuations._ 

trait Scribble[Resource] {
  def wrapWithCatch(
    sk : Option[Resource] => Unit @suspendable
  ) : Option[Resource] => Unit @suspendable = {
    ( optRsrc ) => {
      try {
	sk( optRsrc )
      }
      catch {
        case t : Throwable => {
          val errors : java.io.StringWriter = new java.io.StringWriter()
          t.printStackTrace( new java.io.PrintWriter( errors ) )
          println( "unhandled exception : " + errors.toString( ) );                
        }
      }
    }
  }
}
