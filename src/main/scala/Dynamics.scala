package provingGround

import Stream._
import Structures._
import annotation.tailrec
import scala.util._
import scala.language.postfixOps

import akka.actor._
import akka.pattern.ask
import scala.concurrent._
import scala.concurrent.duration._

import provingGround.HoTT._

import ExecutionContext.Implicits.global

object Dynamics{
    trait DynSys[A]{
        def act: Set[A] => DynState[A]
                
        
        def mixin[B](isle: => DynIsle[A, B]): DynSys[A] = DynDynSys ((s : Set[A]) => {
            val nextIsle = isle.next
            DynState(s union (nextIsle.state map (isle.mapBy)), mixin(nextIsle))
        }        
        )
        
        def mixinSet[B](isles: => Set[DynIsle[A, B]]): DynSys[A] = DynDynSys ((s : Set[A]) => {
            val nextIsles = isles map (_.next)
            val export = for (isle <- nextIsles; b <- isle.state) yield (isle.mapBy(b)) 
            DynState(s union export, mixinSet(nextIsles))
        }        
        )
        
        def spawn[B](isle: => DynIsle[A, B]) = DynDynSys ((s: Set[A]) => {
            DynState(act(s).state, act(s).dyn mixin isle)
        }
        )
        
        def spawnSet[B](isles:  Set[A] => Set[DynIsle[A, B]]) = DynDynSys ((s: Set[A]) => {
            DynState(act(s).state, act(s).dyn mixinSet isles(s))
        }
        )
    }
        
    object DynSys{
     def id[A] = StatDynSys((s: Set[A]) => s)
     
     def lift[A](f: A => A) = StatDynSys((s: Set[A]) => (s map f))
     
     def plift[A](pf: PartialFunction[A,A]) = StatDynSys((s: Set[A]) => (s collect pf))
     
     def pairs[A](pairing: PartialFunction[(A, A), A]) = StatDynSys(
       (s : Set[A]) => (for(x<- s; y<-s) yield (x,y)) collect pairing
     )
     
     def triples[A](tripling: PartialFunction[(A, A, A), A]) = StatDynSys(
       (s : Set[A]) => (for(x<- s; y<-s; z <-s) yield (x,y, z)) collect tripling
     )

	def apply[A](pf: PartialFunction[A, A]) = StatDynSys((s: Set[A])=> s collect pf)
    }
    
    case class DynState[A](state: Set[A], dyn: DynSys[A]){
        def next = dyn.act(state)
        
        @tailrec final def recSteps(n: Int, sofar: DynState[A] = this): DynState[A] = {
            if (n<1) sofar else recSteps(n-1, sofar.next)        
        }
        
        def get(n: Int) = recSteps(n: Int).state
    }
    
    case class DynDynSys[A](act: Set[A] => DynState[A]) extends DynSys[A]
    
    
    case class StatDynSys[A](step: Set[A] => Set[A]) extends DynSys[A] with Function1[Set[A], Set[A]]{
      
        def act = (s: Set[A]) => DynState(step(s), this)   

        def apply(s: Set[A]) = step(s)
     
        def union(thatStep: Set[A] => Set[A]) = StatDynSys((s: Set[A])=> step(s) union thatStep(s))
     
        def ++(thatStep: Set[A] => Set[A]) = union(thatStep)
     
        def andThen(thatStep: Set[A] => Set[A]) = StatDynSys((s: Set[A]) => thatStep(step(s)))
     
    }
    
    case class DelaySys[A](dyn: DynSys[A]) extends DynSys[A]{
        def act = (s: Set[A]) => DynState(s, dyn)
    }
    
    case class DynIsle[A, B](isleState: DynState[B], mapBy: B => A){
        def next = DynIsle(isleState.next, mapBy)
        def state = isleState.state
    }
    
    object HottDyn{
       type Pairing = PartialFunction[(AbsObj, AbsObj),AbsObj]
    
       def Applications[W<: AbsObj, V<: Typ[U], U<: AbsObj]: Pairing = {
           case (f: FuncObj[_, _, _], x: AbsObj) if f.dom == x.typ => f(x).get 
       }

	
       def LogicalArrows[V <: AbsObj]: Pairing = {
           case (dom: LogicalTyp, codom: Typ[_]) => dom --> codom
		    }

	   def lambdaIsles(dyn:  => DynSys[AbsObj])(state: Set[AbsObj]) ={
	     val newVarSym = nextChar(usedChars(state))
	     val gens: PartialFunction[AbsObj, DynIsle[AbsObj, AbsObj]] = {
	       case typ: Typ[_] => 
	         val obj = typ.symbObj(newVarSym)
	         DynIsle(DynState(state+obj, dyn), lambda(obj) _)
	     }
	     state collect gens
	   }
			
	val InferenceDyn : DynSys[AbsObj] = DynSys.id[AbsObj] ++ DynSys.pairs(LogicalArrows) spawnSet(lambdaIsles(InferenceDyn) _)
	
//	val InferenceDyn = Dyn.id[AbsObj] ++ Dyn.pairs(LogicalArrows) andThen (expandGens _) mixin (lambdaGens _) 
    }
    
}
