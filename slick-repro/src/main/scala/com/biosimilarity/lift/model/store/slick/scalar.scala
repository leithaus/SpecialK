// -*- mode: Scala;-*- 
// Filename:    scalar.scala 
// Authors:     lgm                                                    
// Creation:    Sat Dec 29 06:29:48 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store.slick

import scala.slick.driver.H2Driver
import scala.slick.driver.H2Driver.simple._
import scala.slick.direct._
//import scala.slick.direct.AnnotationMapper._
import scala.reflect.runtime.universe
import scala.slick.jdbc.StaticQuery.interpolation

object ScalarLanguageSchema {
  object Nominals extends Table[(Int,String,Int,Int)]( "NOMINALS" ) {
    def id = column[Int]( "NOMINAL_ID", O.PrimaryKey )
    def identifier = column[String]( "IDENTIFIER_NAME" )
    def reference = column[Int]( "TERM_REFERENCE" )
    def nominalType = column[Int]( "NOMINAL_TYPE" )

    // project operator required by Slick
    def * = id ~ identifier ~ reference ~ nominalType

    // foreign key access to reference
    def nominal = foreignKey( "REFERENCE_FK", reference, Expressions )( _.id )
  }
  object NominalType extends Enumeration {
    type NominalType = Value
    val IdentifierVar, ReferenceVar = Value
  }

  object Mentions extends Table[(Int,Int)]( "MENTIONS" ) {
    def id = column[Int]( "MENTION_ID", O.PrimaryKey )
    def identifier = column[Int]( "IDENTIFIER_ID" )

    // project operator required by Slick
    def * = id ~ identifier

    // foreign key access into nominals
    def nominal = foreignKey( "NOMINAL_FK", identifier, Nominals )( _.id )
  }

  object Abstractions extends Table[(Int,Int,Int)]( "ABSTRACTIONS" ) {
    def id = column[Int]( "ABSTRACTION_ID", O.PrimaryKey )
    def parameters = column[Int]( "PARAMS_ID" )
    def code = column[Int]( "CODE_ID" )

    // project operator required by Slick
    def * = id ~ parameters ~ code

    // foreign key access to formals and body
    def formals = foreignKey( "FORMALS_FK", parameters, Mentions )( _.id )
    def body = foreignKey( "BODY_FK", code, Expressions )( _.id )
  }

  object Applications extends Table[(Int,Int,Int)]( "APPLICATIONS" ) {
    def id = column[Int]( "APPLICATION_ID", O.PrimaryKey )
    def function = column[Int]( "FUNCTION_ID" )
    def parameters = column[Int]( "PARAMS_ID" )

    // project operator required by Slick
    def * = id ~ function ~ parameters

    // foreign key access to operation and actuals
    def operation = foreignKey( "OP_FK", function, Expressions )( _.id )
    def actuals = foreignKey( "ACTUALS_FK", parameters, Mentions )( _.id )
  }

  object Dereferences extends Table[(Int,Int)]( "DEREFERENCES" ) {
    def id = column[Int]( "DEREF_ID", O.PrimaryKey )
    def ref = column[Int]( "NOMINAL_REF" )

    // project operator required by Slick
    def * = id ~ ref

    // foreign key access into nominals
    def nominal = foreignKey( "NOMINAL_FK", ref, Nominals )( _.id )
  }

  // This table represents an indirection from ``pointers'' (aka expression id's) 
  // to expressions. It actually represents the disjoint union
  // Mention | Abstraction | Application
  // So, each element in a row is going to have one of those columns inhabited
  // and the others ``nulled''
  object Expressions extends Table[(Int,Int,Int,Int,Int)]( "EXPRESSIONS" ){
    def id = column[Int]( "EXPRESSION_ID", O.PrimaryKey )

    // the disjoint union
    def mention = column[Int]( "MENTION_ID" )
    def abstraction = column[Int]( "ABSTRACTION_ID" )
    def application = column[Int]( "APPLICATION_ID" )

    // the discriminator of the disjoint union
    def expressionType = column[Int]( "EXPRESSION_TYPE" )

    // project operator required by Slick
    def * = id ~ mention ~ abstraction ~ application ~ expressionType

    // foreign key access to the actual expression
    def toMention =
      foreignKey( "MENTION_FK", mention, Mentions )( _.id )
    def toAbstraction =
      foreignKey( "ABSTRACTION_FK", mention, Abstractions )( _.id )
    def toApplication =
      foreignKey( "APPLICATION_FK", mention, Applications )( _.id )
  }

  object ExpressionType extends Enumeration {
    type ExpressionType = Value
    val DivergenceExpr, MentionExpr, AbstractionExpr, ApplicationExpr, DereferenceExpr = Value
  }
}

object ExerciseScalarLanguageSchema {
  // ( ( x ) => *x ) @( ( x ) => *x )
  import ScalarLanguageSchema._
  import NominalType._
  import ExpressionType._
  import Database.threadLocalSession

  import java.util.UUID

  implicit val randomizer : Int => Int = ( i : Int ) => i
  def getFreshId( implicit rndmzr : Int => Int ) : Int = {
    val uuidV = UUID.randomUUID.toString.split( "-" )
    val uuidComp = uuidV( 4 )
    val seed = uuidComp.substring( 0, uuidComp.length - 4 )
    randomizer( Integer.parseInt( seed ) )
  }

  def freshId : Int = getFreshId( randomizer )  

  // These table definitions have to be dynamically created for each
  // compilation unit
  // How do we link dynamically generated tables to their expression
  // components?
  // What about something simple, like <ComponentType>_<ExpressionId>

  val exprs =
    (
      freshId, // application
      freshId, // left abstraction
      freshId, // right abstraction
      freshId, // left dereference
      freshId, // right dereference
      freshId, // right first actual mention
      freshId  // extra freshness, just in case...
    );

  object FormalsLeft extends Table[(Int,Int)]( exprs._2 + "_" + "FORMALS" ) {
    def id = column[Int]( "FORMAL_ID", O.PrimaryKey )
    def formal = column[Int]( "NOMINAL_ID" )
    
    // project operator required by Slick
    def * = id ~ formal

    // foreign key access into nominals
    def nominal = foreignKey( "NOMINAL_FK", formal, Nominals )( _.id )
  }
  object ActualsLeft extends Table[(Int,Int)]( exprs._1 + "_" + "ACTUALS" ) {
    def id = column[Int]( "EXPRESSION_LOCATION", O.PrimaryKey )
    def ptr = column[Int]( "EXPRESSION_PTR" )
    
    // project operator required by Slick
    def * = id ~ ptr

    // foreign key access into mentions
    def mention = foreignKey( "MENTION_FK", ptr, Mentions )( _.id )
  }
  object FormalsRight extends Table[(Int,Int)]( exprs._3 + "_" + "FORMALS" ) {
    def id = column[Int]( "MENTION_ID", O.PrimaryKey )
    def formal = column[Int]( "NOMINAL_ID" )
    
    // project operator required by Slick
    def * = id ~ formal

    // foreign key access into nominals
    def nominal = foreignKey( "NOMINAL_FK", formal, Nominals )( _.id )
  }

  
  Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {
    // The session is never named explicitly. It is bound to the current
    // thread as the threadLocalSession that we imported

    // Create the tables, including primary and foreign keys

    (
      Mentions.ddl
      ++ Abstractions.ddl
      ++ Applications.ddl
      ++ Expressions.ddl
      ++ FormalsLeft.ddl
      ++ ActualsLeft.ddl
      ++ FormalsRight.ddl
    ).create
    
    val xes = ( freshId, freshId );
    val mntns = ( freshId, freshId );
    val derefs = ( freshId, freshId );
    val abs = ( freshId, freshId );
    val apps = new Tuple1( freshId );    

    Nominals.insertAll(
      ( xes._1, "x", -1, IdentifierVar.id ),     // in the abstraction bodies
      ( xes._2, "", exprs._6, ReferenceVar.id )  // in the application actuals
    )
        
    Mentions.insertAll(                       // red herring
      ( mntns._1, xes._1 ),
      ( mntns._2, xes._2 )
    )
    Dereferences.insertAll(
      // *x
      ( derefs._1, xes._1 ),
      // *x
      ( derefs._2, xes._1 )
    )
    Abstractions.insertAll(
      // ( ( x ) => *x )
      ( abs._1, -1, exprs._3 ),
      // ( ( x ) => *x )
      ( abs._2, -1, exprs._4 )
    )
    Applications.insertAll(
      // ( ( x ) => *x ) @( ( x ) => *x )
      ( apps._1, exprs._5, -1 )
    )
    Expressions.insertAll(
      ( exprs._1, mntns._1, -1, -1, MentionExpr.id ),      // red herring
      ( exprs._2, mntns._2, -1, -1, MentionExpr.id ),      // red herring

      // *x
      ( exprs._3, derefs._1, -1, -1, DereferenceExpr.id ),

      // *x
      ( exprs._4, derefs._2, -1, -1, DereferenceExpr.id ),
      
      // ( ( x ) => *x )
      ( exprs._5, -1, abs._1, -1, AbstractionExpr.id ),

      // ( ( x ) => *x )
      ( exprs._6, -1, abs._2, -1, AbstractionExpr.id ),

      // ( ( x ) => *x ) @( ( x ) => *x )
      ( exprs._7, -1, -1, apps._1, ApplicationExpr.id )
    )

    def showApplicationExprs : Unit = {
      /*
      for (
	expr <- Expressions if expr.expressionType === ApplicationExpr.id
      ) {
	println( "expr " + expr + " is " + "an application." )
      }
      */
    }
      
  }
}
