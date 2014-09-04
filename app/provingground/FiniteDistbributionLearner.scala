package provingground

import LearningSystem._
import Collections._
import annotation._

object FiniteDistbributionLearner {
	type FD[V] = FiniteDistribution[V] 
	
	type DF[X, Y] = DiffbleFunction[X, Y]
	
	def IdMV[M, V] = DiffbleFunction((mv : (FD[M], FD[V])) => mv)( 
	      (mv : (FD[M], FD[V])) => {(mw : (FD[M], FD[V])) => mw}
	    )
	    
	def IdV[V] = DiffbleFunction((d : FD[V]) => d)( 
	      (d : FD[V]) => {(w : FD[V]) => w}
	    )
	
	@tailrec def repeat[M, V](fn: DF[(FD[M], FD[V]), (FD[M], FD[V])],
	    n: Int, accum: DF[(FD[M], FD[V]), (FD[M], FD[V])]= IdMV): DF[(FD[M], FD[V]), (FD[M], FD[V])] = {
		if (n<1) accum else repeat(fn, n-1, accum andThen fn)
	}
	    
	/**
	 * differentiable function from a given one by composing with scalar multiplication
	 */
	def scProdFn[V](fn : DF[FD[V], FD[V]]) = {
	  def func(cv: (Double, FD[V])) = fn(cv._2) * cv._1
	  
	  def grad(cv: (Double, FD[V]))(w: FD[V]) = {
	    val x = fn.grad(cv._2)(w)
	    (x dot cv._2, x * cv._1)
	  } 
	  
	  DiffbleFunction(func)(grad)
	}
	
	type DynFn[M, V] = DF[(FD[M], FD[V]), FD[V]]
	
	def dstsum[M, V](fst: (FD[M], FD[V]), scnd: (FD[M], FD[V])) = (fst._1 ++ scnd._1, fst._2++ scnd._2)
	
	def sumFn[M, V](fst: DynFn[M, V], scnd : DynFn[M, V]) = {
	  def func(st : (FD[M], FD[V])) = fst(st) ++ scnd(st)
	  
	  def grad(st: (FD[M], FD[V]))(w: FD[V]) ={
	    dstsum(fst.grad(st)(w), scnd.grad(st)(w))
	  }
	  
	  DiffbleFunction(func)(grad)
	}
	
	def atM[M, V](cv: (Double, FD[V]), m: M) = {
	  val dstM = FiniteDistribution.empty[M] + (m, cv._1)  
	  (dstM, cv._2)
	}
	
	def mixinFn[M, V](m: M, fn: DF[(Double, FD[V]), FD[V]]) ={
	  def func(csv: (FD[M], FD[V])) = {
	    val c = csv._1(m)
	    val v = csv._2
	    fn((c, v))
	  }
	  
	  def grad(csv: (FD[M], FD[V]))(w: FD[V]) = {
	    val c = csv._1(m)
	    val v = csv._2
	    val cterm = fn.grad((c, v))(w)
	    atM(cterm, m)
	  }
	  
	  DiffbleFunction(func)(grad)
	}
	
	def isleMultFn[M, V](fn: DynFn[M, V], m: M) = {
	  def func(csv: (FD[M], FD[V])) = {
	    val c = csv._1(m)
	    val v = csv._2
	    fn((csv._1, v)) * c
	  }
	  
	  def grad(csv: (FD[M], FD[V]))(w: FD[V]) = {
	    val c = csv._1(m)
	    val v = csv._2
	    val mv = fn.grad((csv._1, v))(w)
	    val shift = mv._2 dot v
	    (mv._1 + (m, shift), mv._2)
	  }
	  
	  DiffbleFunction(func)(grad)
	}
	
	def pair[V] = {
	  def func(d: FD[V]) = {
	    val pmf= (for (x <- d.support; y <- d.support) yield Weighted((x, y), d(x) * d(y))).toSeq
	    FiniteDistribution(pmf)
	  }
	  
	  def grad(d: FD[V])(pd: FD[(V, V)]) = {
	    val rawpmf = pd.pmf.flatMap(ab => Seq(Weighted(ab.elem._1, d(ab.elem._2)), Weighted(ab.elem._2, d(ab.elem._1))))
	    FiniteDistribution(rawpmf).flatten
	  }
	  
	  DiffbleFunction(func)(grad)
	}
	
	def formalSmooth[A](f: A => A) = DiffbleFunction(f)((a : A) => {(b : A) => b})
	
	def moveFn[V](f: V => Option[V]) = {
	  def func(d: FD[V]) = {
	    val rawpmf = for (x<- d.support.toSeq; y<- f(x)) yield Weighted(y, d(x))
	    FiniteDistribution(rawpmf).flatten
	  }
	  
	  def grad(d: FD[V])(w: FD[V]) = {
	    val rawpmf = for (x<- d.support.toSeq; y<- f(x)) yield Weighted(y, w(y))
	    FiniteDistribution(rawpmf).flatten
	  }
	  
	  DiffbleFunction(func)(grad)
	}
	
	def simpleIsleInitFn[M, V](v: V, t: M) = {
	  def func(ds: (FD[M], FD[V])) ={
	    val p = ds._1(t)
	    (ds._1, ds._2 * (1.0 - p) + (v, p))
	  }
	  
	  def grad(ds: (FD[M], FD[V]))(ws: (FD[M], FD[V])) ={
	    val p = ds._1(t)
	    val c = ws._1(t)
	    val shift = (ds._2 filter ((x: V) => x != v)) * (1.0 - p)
	    (ws._1 + (t, c), shift)
	  }
	  
	  DiffbleFunction(func)(grad)
	}
	
	def extendM[M, V](fn: DF[(FD[M], FD[V]), FD[V]]) = {
	  def func(mv: (FD[M], FD[V])) = (mv._1, fn(mv))
	  
	  def grad(mv: (FD[M], FD[V]))(mw: (FD[M], FD[V])) = (mw._1 ++ fn.grad(mv)(mw._2)._1, fn.grad(mv)(mw._2)._2)
	}
	
	def pruneV[M, V](t: Double) = formalSmooth((mv: (FD[M], FD[V])) => (mv._1, mv._2 normalized(t)))
	
	def prune[V](t: Double) = formalSmooth((d: FD[V]) => d normalized(t))
}