// -*- mode: Scala;-*- 
// Filename:    cosandbox.scala 
// Authors:     lgm                                                    
// Creation:    Tue Aug 14 17:32:36 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.macros.sandbox

import scala.language.experimental.macros

import scala.reflect.makro.Context
import scala.reflect.runtime.{universe => ru}
import scala.reflect.ClassTag

/* ------------------------------------------------------------------------ */
/* This is work in progress toward a scala rendering of co-do               */
/* notation. Currently, the types are all wrong because the macros are      */
/* simply identity macros -- to hold the place of what should be there      */
/* eventually.                                                              */
/* ------------------------------------------------------------------------ */

abstract class BaseCoQueryableFactory{
  //def factory[S]( projection:ru.Expr[BaseQueryable[S]] ) : BaseQueryable[S]  
}

object CoQueryable extends BaseCoQueryableFactory{
  def apply[T](q:CoQueryable[T]) = new CoQueryable[T](q.expr_or_typetag) // TODO: make this a macro
  def apply[T:ru.TypeTag:ClassTag] = new CoQueryable[T](Right( (implicitly[ru.TypeTag[T]],implicitly[ClassTag[T]]) ))
  def factory[S]( projection:ru.Expr[BaseCoQueryable[S]] ) : CoQueryable[S] = {
    new CoQueryable[S]( Left(projection) )
  }
}

object CoQueryableMacros {
  def coUnitM[T:c.TypeTag](
    c : scala.reflect.makro.Context
  )(
    coProjection : c.Expr[CoQueryable[T]]
  ) : c.Expr[CoQueryable[T]] = {
    c.universe.reify{ coProjection.splice }
  }

  def coFlatMapM[T:c.TypeTag, S:c.TypeTag](
    c : scala.reflect.makro.Context
  )(
    coProjection : c.Expr[CoQueryable[T] => S]
  ) : c.Expr[CoQueryable[T] => S] = {
    c.universe.reify{ coProjection.splice }
  }

  def filter[T:c.TypeTag](
    c : scala.reflect.makro.Context
  )(
    coProjection : c.Expr[T => Boolean]
  ) : c.Expr[T => Boolean] = {
    c.universe.reify{ coProjection.splice }
  }

  def desugarC4[T:c.TypeTag, S:c.TypeTag](
    c : scala.reflect.makro.Context
  )(
    coProjection : c.Expr[CoQueryable[T] => S]
  ) : c.Expr[CoQueryable[T] => S] = {
    coProjection
  }  
}

class CoQueryableValue[T]( val value : ru.Expr[T] )
abstract class BaseCoQueryable [T]( val expr_or_typetag : Either[ ru.Expr[_], (ru.TypeTag[_],ClassTag[_]) ] ){
  def queryable = this
}

object CoQueryOps{
  def query[T]( queryable:BaseCoQueryable[T] ) : QueryOps[T] = ???
}
class CoQueryOps[T]{
  def coFlatMap[S]( coProjection: T => BaseCoQueryable[S] ) : BaseCoQueryable[S] = ???
  def filter( coProjection: T => Boolean ) : BaseCoQueryable[T] = ???
}

class CoQueryable[T]( expr_or_typetag : Either[ ru.Expr[_], (ru.TypeTag[_],ClassTag[_]) ] ) extends BaseCoQueryable[T]( expr_or_typetag ){
  // type should be [S]( cp : CoQueryable[S] ) : S
  def coUnit[S]( coProjection : CoQueryable[S] ) : CoQueryable[S] = macro CoQueryableMacros.coUnitM[S]
  // type should be [S]( cp : CoQueryable[T] => S ) : CoQueryable[S] 
  def coFlatMap[S]( coProjection : CoQueryable[T] => S ) : CoQueryable[T] => S = macro CoQueryableMacros.coFlatMapM[T,S]
  // type should be ( cp : T => Boolean ) : CoQueryable[T]
  def filter( coProjection : T => Boolean ) : T => Boolean = macro CoQueryableMacros.filter[T]
  def withFilter( coProjection : T => Boolean ) : T => Boolean = macro CoQueryableMacros.filter[T]  

  /* ------------------------------------------------------------------------ */
  /*                       co-do notation desugaring                          */
  /* ------------------------------------------------------------------------ */

  def cfor[S]( coProjection : CoQueryable[T] => S ) : CoQueryable[T] => S = macro CoQueryableMacros.desugarC4[T,S]
}
