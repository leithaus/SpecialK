// -*- mode: Scala;-*- 
// Filename:    MonadicJunction.scala 
// Authors:     lgm                                                    
// Creation:    Fri Jan 21 20:36:16 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._
import net.liftweb.amqp._

import scala.util.continuations._ 
import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack

import com.rabbitmq.client._

import org.prolog4j._

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import java.util.UUID
import java.net.URI
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream

trait MonadicDTSMsgScope[Namespace,Var,Tag,Value]
extends DTSMsgScope[Namespace,Var,Tag,Value]
{
  
}


package usage {
/* ------------------------------------------------------------------
 * Mostly self-contained object to support unit testing
 * ------------------------------------------------------------------ */ 

}
