package com.biosimilarity.lift.lib.term.conversion

import scala.collection.mutable.HashMap
import scala.language.implicitConversions
import com.biosimilarity.lift.lib.zipper._
import com.biosimilarity.lift.model.store._
import scala.util.parsing.json._

// Tree[A] = Fringe[A]
// TreeItem is an A-labeled leaf, extends Tree[A]
// TreeSection is an unlabeled node, extends Tree[A]

// CnxnLabel[Namespace, Tag] extends Tree[Tag]
// Shorten Namespace to N, Tag to T
// CnxnLeaf[N, T] extends TreeItem[T]
// CnxnBranch[N, T] extends TreeSection[T]

// CnxnCtxtLabel[N,Var,T] extends CnxnLabel[Either[N,Var], Either[T,Var]]
// CnxnCtxtLeaf[N,Var,T]( val T : Either[T,Var] ) extends TreeItem[Either[T,Var]]( T ) with CnxnCtxtLabel[N,Var,T]
// CnxnCtxtBranch[N,Var,T](override val n : N, val factuals : List[CnxnCtxtLabel[N,Var,T] with Factual])
//   extends TreeSection[Either[T,Var]]( factuals ) with AbstractCnxnCtxtBranch[N,Var,T]

// LabeledTreeContext is List(dCnxnCtxtLabel) where the self-reference gives cons

abstract class Eval[X,T](val instance: Location[Either[T, X]]) {
  def eval[Return](k: Location[Either[T, X]] => Return, m: HashMap[String, Location[Either[T, X]] => Return]): Return
  def eval[Return](k: Location[Either[T, X]] => Return): Return = eval(k, new HashMap[String, Location[Either[T, X]] => Return])
}

class LambdaEval(override val instance: Location[Either[String, String]]) extends Eval(instance) {
  class Term
  case class Var (val x: String) extends Term
  case class App (val t1: Term, val t2: Term) extends Term
  case class Lam (val x: String, t1: Term) extends Term

  def render(instance: Term): String = instance match {
    case Var(x) => "var(\"" + JSONFormat.quoteString(x) + "\")"
    case App(t1, t2) => "app(" + render(t1) + ", "+ render(t2) + ")"
    case Lam(x, t1) => "lam(\"" + JSONFormat.quoteString(x) + "\", " + render(t1) + ")"
  }
  assert( render(App(Lam("x", Var("x")), Var("y"))) == """app(lam("x", var("x")), var("y"))""" )

  def cclToTerm(l: CnxnCtxtLabel[String,String,String]): Option[Term] = {
    l match {
      case CnxnCtxtBranch("var", CnxnCtxtLeaf(Left(x)) :: Nil) => Some(Var(x))
      case CnxnCtxtBranch("app", t :: u :: Nil) => for (t1 <- cclToTerm(t); t2 <- cclToTerm(u)) yield App(t1, t2)
      case CnxnCtxtBranch("lam", CnxnCtxtLeaf(Left(x)) :: t :: Nil) => for (t1 <- cclToTerm(t)) yield Lam(x, t1)
      case _ => None
    }
  }
  assert( 
    cclToTerm(
      new CnxnCtxtBranch(
        "app",
        List(
          new CnxnCtxtBranch(
            "lam",
            List(
              new CnxnCtxtLeaf(Left("x")),
              new CnxnCtxtBranch(
                "var",
                List(new CnxnCtxtLeaf(Left("x")))
              )
            )
          ),
          new CnxnCtxtBranch(
            "var",
            List(
              new CnxnCtxtLeaf(Left("y"))
            )
          )
        )
      )
    ) == Some(App(Lam("x", Var("x")), Var("y")))
  )
  
  def termToCCL(t: Term): CnxnCtxtLabel[String,String,String] with Factual = t match {
    case Var(x) => new CnxnCtxtBranch("var", List(new CnxnCtxtLeaf(Left(x))))
    case App(t1, t2) => new CnxnCtxtBranch("app", List(termToCCL(t1), termToCCL(t2)))
    case Lam(x, t1) => new CnxnCtxtBranch("lam", List(new CnxnCtxtLeaf(Left(x)), termToCCL(t1)))
  }
  assert( cclToTerm(termToCCL(App(Lam("x", Var("x")), Var("y")))) == Some(App(Lam("x", Var("x")), Var("y"))) )

  class FreeVars(val instance: Term) {
    def freeVars: List[String] = instance match {
      case Var(x) => List(x)
      case App(t1, t2) => t1.freeVars ++ t2.freeVars
      case Lam(x, t1) => t1.freeVars.filter(y => y != x)
    }
  }
  implicit def addFreeVars(t: Term): FreeVars = new FreeVars(t)
  implicit def noFreeVars(f: FreeVars): Term = f.instance
  assert( Var("x").freeVars == List("x") )
  assert( Lam("x", App(Var("x"), Var("y"))).freeVars == List("y") )


  trait Fresh { 
    def fresh(): String
  }

  implicit val deBruijnStr = new Fresh {
    var counter: Int = 0;
    def fresh(): String = {
      counter += 1;
      counter.toString
    }
  }

  class Subst(val instance: Term)(implicit F: Fresh) {
    def subst(n: String, t: Term): Subst = {
      instance match {
        case v@Var(x) => {
          if (x == n) {
            t
          } else {
            v
          }
        }

        case App(t1, t2) => {
          App(t1.subst(n, t), t2.subst(n, t))
        }

        case l@Lam(x, t1) => {
          if (n == x) {
            l
          } else if (t.freeVars.contains(x)) {
            val f = F.fresh()
            Lam(f, t1.subst(x, Var(f)).subst(n, t))
          } else {
            Lam(x, t1.subst(n, t))
          }
        }
      }
    }
  }
  implicit def addSubst(t: Term): Subst = new Subst(t)
  implicit def noSubst(s: Subst): Term = s.instance
  assert( Var("x").subst("x", Var("y")).instance == Var("y") )
  assert( Lam("x", Var("x")).subst("x", Var("y")).instance == Lam("x", Var("x")) )
  assert( (Lam("x", App(Var("y"), Var("x"))).subst("y", Var("x")).instance match { case Lam(x, _) => x }) != "x" )
  assert( (Lam("x", App(Var("y"), Var("x"))).subst("y", Var("x")).instance match { case Lam(_, App(t, _)) => t }) == Var("x") )


  class Beta(val instance: Lam)(implicit F: Fresh) {
    def beta(t: Term): Term = {
      instance.t1.subst(instance.x, t)
    }
  }
  implicit def addBeta(l: Lam)(implicit F: Fresh): Beta = new Beta(l)
  implicit def noBeta(b: Beta): Lam = b.instance
  assert(Lam("x", Var("x")).beta(Var("y")) == Var("y"))
  assert(Lam("x", App(Var("x"), Var("z"))).beta(Var("y")) == App(Var("y"), Var("z")))

  def eval[Return](
      k: Location[Either[String, String]] => Return,
      m: HashMap[String, Location[Either[String, String]] => Return]
  ): Return = {
    m += (("agent-session://" + instance.ctxt + "/" + instance.tree, k))
    instance.tree match {
      case tree: CnxnCtxtLabel[String,String,String] => cclToTerm(tree) match {
        case Some(tree) => tree match {
          case Var(_) => k(instance)
          case Lam(_, _) => k(instance)
          case App(Var(_), _) => k(instance)
          case App(l@Lam(_, _), t) => new Location[Either[String, String]](termToCCL(l.beta(t)), instance.ctxt).eval(k, m)
          case App(a@App(_, _), t1) => {
            new Location[Either[String, String]](
              termToCCL(a), 
              LabeledTreeContext( "app", List(), instance.ctxt, List(termToCCL(t1)) )
            ).eval(
              (t: Location[Either[String, String]]) => t.tree match {
                case tree: CnxnCtxtBranch[String,String,String] with Factual => Location[Either[String, String]](
                  new CnxnCtxtBranch("app", List(tree, termToCCL(t1))),
                  instance.ctxt
                ).eval(k, m)
              },
              m
            )
          }
        }
      }
    }
  }
  implicit def addLambdaEval[Name](t: Location[Either[String, String]])(implicit F: Fresh): LambdaEval = new LambdaEval(t)
  implicit def noLambdaEval[Name](e: LambdaEval): Location[Either[String, String]] = e.instance
  implicit def addLambdaEvalChain[Name](t: CnxnCtxtLabel[String,String,String])(implicit F: Fresh): LambdaEval = 
      new LambdaEval(Location[Either[String, String]](t, Top()))
  {
    import com.biosimilarity.lift.lib.term.conversion.usage._
    val m = new HashMap[String, Location[Either[String, String]] => Unit]
    Location(TermToCCLStr( ).strToTerm("app(app(lam(x, lam(y, var(x))), var(y)), var(z))"), Top()).eval(
      (t: Location[Either[String, String]]) => t.tree match {
        case tree: CnxnCtxtBranch[String,String,String] with Factual =>
          assert(cclToTerm(tree) == Some(Var("y")))
      }
    )
  }
}


