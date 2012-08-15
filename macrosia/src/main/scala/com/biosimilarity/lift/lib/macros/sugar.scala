// -*- mode: Scala;-*- 
// Filename:    sugar.scala 
// Authors:     lgm                                                    
// Creation:    Tue Aug 14 16:21:14 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.macros.sandbox

import scala.language.experimental.macros

import scala.reflect.makro.Context
import scala.reflect.runtime.{universe => ru}
import scala.reflect.ClassTag

object Sugareedoo {
  import CoQueryableMacros._
  class Msg[T](
    val b : Boolean,
    val i : Int,
    val s : String,
    val t : T,
    val r : Option[Msg[T]]
  )
  object Msg {
    def apply[T](
      b : Boolean,
      i : Int,
      s : String,
      t : T,
      r : Option[Msg[T]]
    ) : Msg[T] = {
      new Msg[T]( b, i, s, t, r )
    }
    def unapply[T](
      msgT : Msg[T]
    ) : Option[( Boolean, Int, String, T, Option[Msg[T]] )] = {
      Some( ( msgT.b, msgT.i, msgT.s, msgT.t, msgT.r ) )
    }
  }

  case class Weird(
    override val b : Boolean,
    override val i : Int,
    override val s : String,
    override val t : Option[Weird],
    override val r : Option[Msg[Option[Weird]]]
  ) extends Msg[Option[Weird]]( b, i , s, t, r )

  lazy val sfunOne = {
    ( cqry1 : CoQueryable[Weird] ) => { cqry1 + "" }
  }
  
  lazy val cq1 : CoQueryable[Weird] = CoQueryable[Weird]
 
  //def splode() = cq1.cfor[String]( sfunOne )
 
}
