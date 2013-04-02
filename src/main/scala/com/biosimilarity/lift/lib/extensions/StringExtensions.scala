package com.biosimilarity.lift.lib.extensions

import java.net.URI
import com.biosimilarity.lift.lib.moniker._
import com.biosimilarity.lift.model.store._


object StringExtensions
  extends CnxnString[String,String,String]
{
  implicit def stringExt(s: String) = new StringExt(s)

  class StringExt(source: String) {

    def toLabel: CnxnCtxtLabel[String,String,String] = {
      fromTermString( source ) match {
        case Some( ccl ) => ccl
        case None => throw new Exception( "failed to parse" )
      }
    }

  }
}