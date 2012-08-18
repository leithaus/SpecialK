// -*- mode: Scala;-*- 
// Filename:    macro.scala 
// Authors:     lgm                                                    
// Creation:    Fri Aug 17 15:49:43 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

//import com.biosimilarity.lift.lib.macros.sandbox._

import scala.language.experimental.macros

import scala.reflect.makro.Context
import scala.reflect.runtime.{universe => ru}
import scala.reflect.ClassTag

trait MonadicComprehension[M[_]] {
  type Brane[A] <: Membrane[A]
  implicit def typeToBrane [A] ( ma : M[A] ) : Brane[A]
  implicit def typeToFilteredBrane [A] ( ma : M[A] ) : Brane[A] with Filter[A]
  implicit def braneToType [A] ( ma : Membrane[A] ) : M[A]
  implicit def ambientMonad : Monad[M]
  
  trait Membrane[A] {
    def flatMap [B] (
      f : A => Brane[B]
    )( implicit mm : Monad[M] ) : Brane[B] = {
      typeToBrane[B](
	mm.bind(
	  braneToType[A]( this ),
	  ( a : A ) => braneToType( f( a ) )
	)
      )
    }
    def foreach ( f : A => Unit ) : Unit = {
      map[Unit]( f )
    }
    def map [B] ( f : A => B )( implicit mm : Monad[M] ) : Brane[B] = {
      typeToBrane[B]( mm.fmap( f )( braneToType[A]( this ) ) )
    }
  }

  // For adding for-notation filter behavior to the mix
  trait Filter[A] {
    self : Membrane[A] =>
      def withFilter(
	pred : A => Boolean
      )( implicit mm : Monad[M] ) : Brane[A] with Filter[A] = {
	val fn =
	  ( a : A ) => { 
	    pred( a ) match {
	      case true => a
	      case false =>
		throw new Exception(
		  "default filter failed: " + a + " , " + pred
		)
	    }
	  }
	typeToFilteredBrane( mm.fmap( fn )( braneToType( this ) ) )
      }
    def filter( pred : A => Boolean ) : Brane[A] with Filter[A] = {
      withFilter( pred )
    }
  }
}

trait ScalaMonadicComprehension[M[_]]
extends MonadicComprehension[M] 
with BMonad[M] {
  class Cell[A]( val ma : M[A] )
  extends Membrane[A] with Filter[A]
  object Cell {
    def apply [A] ( ma : M[A] ) : Cell[A] = { new Cell[A]( ma ) }
    def unapply [A] ( ca : Cell[A] ) : Option[ ( M[A] ) ] = { Some( ca.ma ) }
  }   

  type Brane[A] = Cell[A]
  implicit def typeToBrane [A] ( ma : M[A] ) : Brane[A] = {
    Cell[A]( ma )
  }
  implicit def typeToFilteredBrane [A] (
    ma : M[A]
  ) : Brane[A] with Filter[A] = {
    Cell[A]( ma )
  }
  implicit def braneToType [A] ( mba : Membrane[A] ) : M[A] = {
    mba match {
      case Cell( ma ) => ma
      case _ => throw new Exception( "Brane and Brane what is Brane? " + mba )
    }
  }
  implicit def ambientMonad : Monad[M] = this
}

case class CollectedState[A]( s : List[A] ) // no comprehension support

// import CollectedMonadicComprehension._
// for comprehension support
object CollectedMonadicComprehension
     extends ScalaMonadicComprehension[CollectedState]
{  
  override def unit [A] ( a : A ) : CollectedState[A] = {
    CollectedState[A]( List[A]( a ) )
  }
  override def bind [A,B](
    ma : CollectedState[A], f : A => CollectedState[B]
  ) : CollectedState[B] = {
    CollectedState[B]( ( List[B]( ) /: ma.s )( ( acc, e ) => acc ++ f( e ).s ) )
  }  
}
