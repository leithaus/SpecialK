// -*- mode: Scala;-*- 
// Filename:    MonadicTermSpace.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 31 01:46:38 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.util.continuations._ 
import scala.xml._
import scala.collection.MapProxy
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer

import org.prolog4j._

//import org.exist.storage.DBBroker

import org.xmldb.api.base.{ Resource => XmlDbRrsc, _}
import org.xmldb.api.modules._
import org.xmldb.api._

//import org.exist.util.serializer.SAXSerializer
//import org.exist.util.serializer.SerializerPool

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import javax.xml.transform.OutputKeys
import java.util.Properties
import java.net.URI
import java.io.File
import java.io.FileInputStream
import java.io.OutputStreamWriter

trait DistributedAskTypes {
  trait Ask
  case object AGet extends Ask
  case object AFetch extends Ask
  case object ASubscribe extends Ask    
  
  // workaround due to bug in scala runtime
  type AskNum = Int
  val AGetNum : AskNum = 0
  val AFetchNum : AskNum = 1
  val ASubscribeNum : AskNum = 2
}

trait DistributedAskTypeScope {
  type DATypes <: DistributedAskTypes
  def protoAskTypes : DATypes
  val dAT : DATypes = protoAskTypes
}

trait MonadicSoloTermStoreScope[Namespace,Var,Tag,Value] 
extends MonadicTermTypeScope[Namespace,Var,Tag,Value] 
  with MonadicDTSMsgScope[Namespace,Var,Tag,Value]
  with DistributedAskTypeScope
{          

  trait MonadicTermStoreT
   extends MonadicTupleSpace[mTT.GetRequest,mTT.GetRequest,mTT.Resource] 
    with CnxnCtxtInjector[Namespace,Var,Tag]
    with CnxnUnificationCompositeTermQuery[Namespace,Var,Tag]
    with CnxnConversions[Namespace,Var,Tag]
    with WireTap
    with Journalist
    with ConfiggyReporting
    with ConfiguredJournal
    with ConfigurationTrampoline
    with UUIDOps
    with Serializable
  {
    override def tap [A] ( fact : A ) : Unit = {
      reportage( fact )
    }
    
    override lazy val theMeetingPlace =
      new mTT.TMapR[Namespace,Var,Tag,Value]()
    override lazy val theChannels =
      new mTT.TMapR[Namespace,Var,Tag,Value]()
    override lazy val theWaiters =
      new TMapK[Namespace,Var,Tag,Value]()
    override lazy val theSubscriptions =
      new TMapK[Namespace,Var,Tag,Value]()

    class TMapK[Namespace,Var,Tag,Value]
    extends HashMap[mTT.GetRequest,List[RK]]

    case class PrologSubstitution( soln : LinkedHashMap[Var,CnxnCtxtLabel[Namespace,Var,Tag]] )
	 extends Function1[mTT.Resource,Option[mTT.Resource]] {
	   override def apply( rsrc : mTT.Resource ) = {
	     soln.isEmpty match {
	       case true => Some( rsrc )
	       case _ => Some( mTT.RBoundHM( Some( rsrc ), Some( soln ) ) )
	     }
	   }
	 }

    override type Substitution = PrologSubstitution
    
    override def representative(
      ptn : mTT.GetRequest
    ) : mTT.GetRequest = {
      ptn
    }
    override def fits(
      ptn : mTT.GetRequest,
      place : mTT.GetRequest
    ) : Boolean = {
      matchMap( ptn, place ) match {
	case Some( soln ) => {
	  true
	}
	case None => {
	  false
	}
      }
    }

    override def fitsK(
      ptn : mTT.GetRequest,
      place : mTT.GetRequest
    ) : Option[Substitution] = {
      val matchRslts = matchMap( ptn, place )
      matchRslts match {
	case Some( soln : LinkedHashMap[Var,CnxnCtxtLabel[Namespace,Var,Tag]] ) => {
	  Some( PrologSubstitution( soln ) )
	}
	case None => {
	  None
	}
      }
    }

    def getGV( rsrc : mTT.Resource ) : Option[Value] = {
      rsrc match {
	case mTT.Ground( v ) => Some( v )
	case mTT.RBoundHM( Some( nrsrc ), _ ) => getGV( nrsrc )
	case _ => None
      }
    }    

    def asCursor(
      values : List[mTT.Resource]
    ) : Option[mTT.Resource] = {
      
      val ig : mTT.Generator[mTT.Resource, Unit, Unit]  = mTT.itergen[mTT.Resource]( values )
	  
      // BUGBUG -- LGM need to return the Solution
      // Currently the PersistenceManifest has no access to the
      // unification machinery
      Some (
        mTT.RBoundHM(
	  Some( mTT.Cursor( ig ) ),
	  None
  	)
      )
    }

    override def mget(
      channels : Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      consume : RetentionPolicy,
      keep : RetentionPolicy
    )( ptn : mTT.GetRequest )( implicit spaceLockKey : Option[Option[mTT.Resource] => Unit @suspendable] )
    : Generator[Option[mTT.Resource],Unit,Unit] = {
      mget( channels, registered, consume, keep, false )( ptn )( spaceLockKey )
    }

    def mget(
      channels : Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      consume : RetentionPolicy,
      keep : RetentionPolicy,
      cursor : Boolean
    )( ptn : mTT.GetRequest )( implicit spaceLockKey : Option[Option[mTT.Resource] => Unit @suspendable] )
    : Generator[Option[mTT.Resource],Unit,Unit] =
      Generator {
	rk : ( Option[mTT.Resource] => Unit @suspendable ) =>
	  shift {
	    outerk : ( Unit => Unit ) =>
	      reset {
		// if ( ! spaceLock.available ) {
		// 		rk( None )
		// 	      }
		// 	      else {
		val slk = spaceLockKey match {
		  case None => Some( rk )
		  case _ => spaceLockKey
		}
		
		spaceLock.occupy( slk )
		
		tweet( "Reader occupying spaceLock on " + this + " for mget on " + ptn + "." )
		tweet( "spaceLock reading room: " + spaceLock.readingRoom )
		tweet( "spaceLock writing room: " + spaceLock.writingRoom )
		
		val map = Left[Map[mTT.GetRequest,mTT.Resource],Map[mTT.GetRequest,List[RK]]]( channels )
		val meets = locations( map, ptn )
		
		if ( meets.isEmpty ) {
		  val place = representative( ptn )
		  tweet( "did not find a resource, storing a continuation: " + rk )
		  tweet( "registered continuation storage: " + registered )
		  tweet( "theWaiters: " + theWaiters )
		  tweet( "theSubscriptions: " + theSubscriptions )		  
		  
		  keep match {
		    case policy : RetainInCache => {
		      registered( place ) =
			registered.get( place ).getOrElse( Nil ) ++ List( rk )
		    }
		    case _ => {
		      tweet( "policy indicates not to retain in cache: " + rk )
		    }
		  }
		  
		  tweet( "stored a continuation: " + rk )
		  tweet( "registered continuation storage: " + registered )
		  tweet( "theWaiters: " + theWaiters )
		  tweet( "theSubscriptions: " + theSubscriptions )
		  
		  keep match {
		    case storagePolicy : RetainInStore => {
		    }
		    case _ => {
		      tweet( "Reader departing spaceLock on " + this + " for mget on " + ptn + "." )
		      spaceLock.depart( slk )
		      tweet( "spaceLock reading room: " + spaceLock.readingRoom )
		      tweet( "spaceLock writing room: " + spaceLock.writingRoom )
		    }
		  }
		  
		  rk( None )
		}
		else {
		  cursor match {
		    case true => {
		      val rsrcRslts : ListBuffer[mTT.Resource] = new ListBuffer[mTT.Resource]()
		      for( placeNRrscNSubst <- meets ) {
			val PlaceInstance( place, Left( rsrc ), s ) = placeNRrscNSubst
			
			tweet( "found a resource: " + rsrc )
			
			rsrcRslts += rsrc
			
			consume match {
			  case policy : RetainInCache => {
			    channels -= place
			  }
			  case _ => {
			    tweet( "policy indicates not to consume from cache: " + place )
			  }
			}
		      	
			//shift { k : ( Unit => Unit ) => k() }
 		      }
		      
		      val rsrcCursor = asCursor( rsrcRslts.toList )
		      
		      keep match {
			case storagePolicy : RetainInStore => {
			}
			case _ => {
			  tweet( "Reader departing spaceLock on " + this + " for mget on " + ptn + "." )
			  spaceLock.depart( slk )
			  tweet( "spaceLock reading room: " + spaceLock.readingRoom )
			  tweet( "spaceLock writing room: " + spaceLock.writingRoom )
			}
		      }
		      
		      rk( rsrcCursor )
		    }
		    case false => {
		      for(
			placeNRrscNSubst <- itergen[PlaceInstance](
			  meets
			)
		      ) {
			val PlaceInstance( place, Left( rsrc ), s ) = placeNRrscNSubst
			
			tweet( "found a resource: " + rsrc )		    
			
			consume match {
			  case policy : RetainInCache => {
			    channels -= place
			  }
			  case _ => {
			    tweet( "policy indicates not to consume from cache: " + place )
			  }
			}
			
			keep match {
			  case storagePolicy : RetainInStore => {
			  }
			  case _ => {
			    tweet( "Reader departing spaceLock on " + this + " for mget on " + ptn + "." )
			    spaceLock.depart( slk )
			    tweet( "spaceLock reading room: " + spaceLock.readingRoom )
			    tweet( "spaceLock writing room: " + spaceLock.writingRoom )
			  }
			}
			
			rk( s( rsrc ) )
			
			//shift { k : ( Unit => Unit ) => k() }
 		      }
		    }
		  }
		  
 		}				
		//}
 		//tweet( "get returning" )
 		outerk()
 	      }
 	  }
      }
    
  }
  
  class MonadicTermStore(
  ) extends MonadicTermStoreT {    
    override def configFileName : Option[String] = None
    override def configurationDefaults : ConfigurationDefaults = {
      ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
    } 
  }

}

trait MonadicTermStoreScope[Namespace,Var,Tag,Value] 
extends MonadicSoloTermStoreScope[Namespace,Var,Tag,Value]  {  
   
}

package usage {
/* ------------------------------------------------------------------
 * Mostly self-contained object to support unit testing
 * ------------------------------------------------------------------ */ 

}
