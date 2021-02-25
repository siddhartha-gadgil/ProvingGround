
import scala.collection.parallel.ParMap
import provingground._ , interface._, HoTT._, learning._ 
import library._, MonoidSimple._
import scala.collection.parallel._

val tg = TermGenParams.zero.copy(appW = 0.2, unAppW = 0.2, goalWeight = 0)
// tg: TermGenParams = TermGenParams(
//   appW = 0.2,
//   unAppW = 0.2,
//   argAppW = 0.0,
//   lmW = 0.0,
//   piW = 0.0,
//   piTermW = 0.0,
//   termsByTypW = 0.0,
//   typFromFamilyW = 0.0,
//   sigmaW = 0.0,
//   recDefW = 0.0,
//   inducDefW = 0.0,
//   typAsCodW = 0.0,
//   targetInducW = 0.0,
//   varWeight = 0.3,
//   goalWeight = 0.0,
//   typVsFamily = 0.5,
//   negTargetW = 0.0,
//   solverW = 0.0,
//   contraW = 0.0,
//   solver = OrElse(
//     fst = OrElse(fst = OrElse(fst = OrElse(fst = <function1>, snd = <function1>), snd = <function1>), snd = <function1>),
//     snd = <function1>
//   )
// )
val state = ParMapState(dist1.toParMap, dist1.map(_.typ).toParMap)
// state: ParMapState = ParMapState(
//   termDist = ParMap(eqM -> 0.3333333333333333, axiom_{eqM(mul(e_l)(a))(a)} -> 0.047619047619047616, axiom_{eqM(mul(a)(e_r))(a)} -> 0.047619047619047616, axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))} -> 0.047619047619047616, e_r -> 0.047619047619047616, axiom_{(eqM(a)(b) \to eqM(b)(a))} -> 0.047619047619047616, axiom_{eqM(a)(a)} -> 0.047619047619047616, mul -> 0.3333333333333333, e_l -> 0.047619047619047616),
//   typDist = ParMap((`a : M ) ~> (((eqM) (((mul) (`a)) (e_r))) (`a)) -> 0.047619047619047616, M -> 0.09523809523809523, (`a : M ) ~> (((eqM) (`a)) (`a)) -> 0.047619047619047616, (M) → ((M) → (M)) -> 0.3333333333333333, (M) → ((M) → (𝒰 _0)) -> 0.3333333333333333, (`a : M ) ~> ((`b : M ) ~> ((((eqM) (`a)) (`b)) → (((eqM) (`b)) (`a)))) -> 0.047619047619047616, (`a : M ) ~> ((`b : M ) ~> ((`c : M ) ~> ((((eqM) (`a)) (`b)) → ((((eqM) (`b)) (`c)) → (((eqM) (`a)) (`c)))))) -> 0.047619047619047616, (`a : M ) ~> (((eqM) (((mul) (e_l)) (`a))) (`a)) -> 0.047619047619047616),
//   vars = Vector(),
//   inds = ParHashMap(),
//   goalDist = ParHashMap(),
//   context = {}
// )
val ns = ParMapState.parNodeSeq(tg)
// ns: TermNodeCoeffSeq[ParMapState] = provingground.learning.TermNodeCoeffSeq$$anon$1@31185cd9
val pde = new ParDistEq(ns.nodeCoeffSeq)
// pde: ParDistEq = provingground.learning.ParDistEq@3ce2071b
val (nextState, eqs) = pde.nextStateEqs(state, math.pow(10, -5))
// nextState: ParMapState = ParMapState(
//   termDist = ParMap(((mul) (e_r)) (((mul) (((mul) (e_l)) (e_l))) (((mul) (e_l)) (e_l))) -> 2.0783396193438968E-5, ((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (((mul) (e_l)) (e_r))) (e_r)) -> 1.9759450936057556E-5, ((eqM) (((mul) (e_r)) (e_r))) (((mul) (e_r)) (e_l)) -> 1.3445889212061274E-4, (axiom_{eqM(mul(a)(e_r))(a)}) (((mul) (e_l)) (((mul) (e_l)) (((mul) (e_l)) (e_l)))) -> 1.5417684162570565E-5, (eqM) (((mul) (e_l)) (((mul) (e_r)) (((mul) (e_r)) (e_r)))) -> 1.0792378913799394E-4, (mul) (((mul) (((mul) (((mul) (e_l)) (e_l))) (e_r))) (((mul) (e_l)) (e_r))) -> 1.5450994979846584E-5, (mul) (((mul) (e_l)) (((mul) (((mul) (e_r)) (e_r))) (((mul) (e_r)) (e_l)))) -> 3.706978104095781E-5, ((mul) (((mul) (e_l)) (((mul) (e_l)) (e_l)))) (((mul) (e_r)) (e_r)) -> 1.8579496519419915E-5, (eqM) (((mul) (((mul) (((mul) (e_r)) (e_l))) (e_l))) (((mul) (e_r)) (e_r))) -> 1.5450994979846584E-5, ((mul) (((mul) (((mul) (e_l)) (e_l))) (((mul) (e_r)) (e_r)))) (e_r) -> 2.731932876785698E-5, (axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (((mul) (e_l)) (((mul) (e_r)) (((mul) (e_l)) (e_r)))) -> 1.5417684162570565E-5, (mul) (((mul) (((mul) (((mul) (e_r)) (e_l))) (e_r))) (e_l)) -> 5.085180905791272E-5, ((eqM) (e_l)) (((mul) (e_l)) (((mul) (e_l)) (((mul) (e_r)) (e_l)))) -> 2.105997882501164E-5, ((mul) (((mul) (e_l)) (e_l))) (e_l) -> 0.0012427427830412152, ((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (e_l)) (((mul) (e_l)) (((mul) (e_l)) (e_l))) -> 1.9401805519355214E-5, ((eqM) (((mul) (e_r)) (e_r))) (((mul) (((mul) (e_l)) (e_r))) (e_r)) -> 4.498203836209794E-5, (mul) (((mul) (((mul) (e_r)) (e_l))) (((mul) (((mul) (e_l)) (e_l))) (e_r))) -> 2.2961067801412867E-5, (mul) (((mul) (((mul) (((mul) (e_r)) (e_r))) (((mul) (e_l)) (e_r)))) (e_r)) -> 1.532673881914226E-5, (`$ajcupq :  M) ↦ (((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (e_l)) (`$ajcupq)) ((axiom_{eqM(a)(a)}) (e_l))) -> 3.0553118004127795E-4, ((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (((mul) (e_l)) (e_r)) -> 1.9208413160087536E-5, ((eqM) (((mul) (e_l)) (e_r))) (((mul) (((mul) (e_r)) (e_r))) (e_r)) -> 4.014289389997996E-5, ((mul) (e_l)) (((mul) (((mul) (e_r)) (e_r))) (((mul) (e_r)) (e_r))) -> 1.1494343382406043E-5, (axiom_{eqM(mul(a)(e_r))(a)}) (((mul) (e_l)) (((mul) (e_l)) (((mul) (e_l)) (e_r)))) -> 1.5417684162570565E-5, (mul) (((mul) (((mul) (e_r)) (e_r))) (((mul) (((mul) (e_l)) (e_l))) (e_l))) -> 2.2961067801412867E-5, ((mul) (((mul) (((mul) (e_l)) (e_l))) (e_r))) (e_r) -> 2.480676817291564E-4, ((mul) (((mul) (e_l)) (e_r))) (((mul) (e_l)) (((mul) (e_l)) (e_r))) -> 4.4167864936622026E-5, ((eqM) (e_r)) (((mul) (((mul) (e_l)) (e_r))) (e_r)) -> 2.8145267478226404E-4, ((eqM) (e_r)) (((mul) (e_r)) (((mul) (((mul) (e_l)) (e_r))) (e_r))) -> 1.5039210352855614E-5, ((mul) (((mul) (e_r)) (e_r))) (((mul) (e_l)) (((mul) (e_r)) (e_r))) -> 4.4167864936622026E-5, (axiom_{eqM(a)(a)}) (((mul) (e_l)) (((mul) (((mul) (e_r)) (e_r))) (e_l))) -> 1.4580557553888363E-5, (mul) (((mul) (e_r)) (((mul) (e_r)) (((mul) (e_l)) (e_r)))) -> 1.0792378913799394E-4, ((eqM) (((mul) (e_l)) (e_l))) (((mul) (e_r)) (((mul) (e_r)) (e_r))) -> 4.4167864936622026E-5, ((mul) (((mul) (e_r)) (e_r))) (e_r) -> 0.0012427427830412152, (axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (((mul) (e_r)) (((mul) (e_r)) (e_l))) -> 9.942632825824327E-5, ((mul) (((mul) (e_l)) (((mul) (e_r)) (e_r)))) (e_r) -> 2.588440390326806E-4, ((mul) (e_l)) (((mul) (e_l)) (((mul) (((mul) (e_l)) (e_r))) (e_l))) -> 1.5039210352855614E-5, (eqM) (((mul) (((mul) (e_r)) (e_r))) (((mul) (e_r)) (((mul) (e_l)) (e_l)))) -> 3.2153257408597545E-5, ((eqM) (((mul) (e_l)) (e_l))) (((mul) (e_r)) (((mul) (e_l)) (e_r))) -> 4.4167864936622026E-5, ((eqM) (e_r)) (((mul) (e_r)) (((mul) (e_r)) (((mul) (e_l)) (e_l)))) -> 2.1059978825011644E-5, ((mul) (e_l)) (((mul) (((mul) (e_l)) (e_r))) (((mul) (e_r)) (e_r))) -> 2.0783396193438968E-5, (eqM) (((mul) (e_r)) (((mul) (e_l)) (((mul) (e_r)) (e_l)))) -> 1.0792378913799394E-4, (eqM) (((mul) (((mul) (((mul) (e_r)) (e_r))) (((mul) (e_r)) (e_l)))) (e_r)) -> 1.5326738819142257E-5, (mul) (((mul) (e_r)) (((mul) (e_r)) (e_r))) -> 0.001308194684222313, eqM -> 0.2000000000000001, (mul) (((mul) (((mul) (e_l)) (((mul) (e_l)) (e_l)))) (((mul) (e_l)) (e_r))) -> 2.163661651551701E-5, ((eqM) (e_r)) (((mul) (((mul) (e_r)) (e_r))) (((mul) (e_r)) (e_l))) -> 1.1494343382406045E-5, ((mul) (e_l)) (((mul) (((mul) (((mul) (e_r)) (e_r))) (e_r))) (e_l)) -> 1.3141200338045464E-5, (axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (((mul) (e_l)) (((mul) (e_l)) (((mul) (e_r)) (e_r)))) -> 1.5417684162570565E-5, (mul) (((mul) (((mul) (((mul) (e_l)) (e_r))) (e_r))) (((mul) (e_l)) (e_r))) -> 1.5450994979846584E-5, (mul) (((mul) (e_l)) (((mul) (((mul) (e_r)) (e_l))) (e_r))) -> 7.70698100130985E-5, (axiom_{eqM(a)(a)}) (((mul) (((mul) (e_l)) (e_r))) (((mul) (e_l)) (e_l))) -> 1.5215202303786562E-5, (axiom_{eqM(a)(a)}) (((mul) (((mul) (e_r)) (e_l))) (((mul) (e_l)) (e_l))) -> 1.5215202303786562E-5, ((eqM) (((mul) (e_r)) (e_l))) (((mul) (e_l)) (e_l)) -> 1.3445889212061274E-4, ((eqM) (((mul) (e_l)) (e_r))) (((mul) (e_r)) (e_r)) -> 1.3445889212061274E-4, (axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (((mul) (e_l)) (((mul) (e_r)) (((mul) (e_r)) (e_r)))) -> 1.5417684162570565E-5, ((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_r)) (e_r))) (e_r) -> 9.089414018680084E-5, ((eqM) (e_r)) (((mul) (((mul) (e_l)) (e_l))) (((mul) (e_r)) (e_r))) -> 2.0783396193438968E-5, ((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (((mul) (e_r)) (e_l))) (e_r)) -> 1.9759450936057556E-5, ((mul) (e_l)) (((mul) (((mul) (e_l)) (e_r))) (((mul) (e_r)) (e_l))) -> 2.0783396193438968E-5, ((mul) (e_l)) (((mul) (((mul) (e_r)) (((mul) (e_r)) (e_r)))) (e_r)) -> 1.38956878418561...
// eqs: ParSet[EquationNode] = <function1>
val thms = nextState.thmsWithProofs
// thms: ParIterable[(Typ[Term], Double, (Term, Double))] = ParArray((((eqM) (((mul) (((mul) (e_l)) (e_r))) (e_r))) (((mul) (((mul) (e_l)) (e_r))) (e_r)),3.532658753698367E-5,((axiom_{eqM(a)(a)}) (((mul) (((mul) (e_l)) (e_r))) (e_r)),1.0125911493191799E-4)), (((eqM) (((mul) (((mul) (e_r)) (e_l))) (e_r))) (((mul) (e_r)) (e_l)),1.5047987030559413E-4,((axiom_{eqM(mul(a)(e_r))(a)}) (((mul) (e_r)) (e_l)),7.160861095995352E-4)), (((eqM) (((mul) (((mul) (e_r)) (e_r))) (e_l))) (((mul) (((mul) (e_r)) (e_r))) (e_l)),3.42166952553264E-5,((axiom_{eqM(a)(a)}) (((mul) (((mul) (e_r)) (e_r))) (e_l)),9.036571162908673E-5)), ((`a : M ) ~> (((eqM) (`a)) (`a)),0.028571428571428577,(axiom_{eqM(a)(a)},0.028571428571428584)), (((eqM) (((mul) (e_l)) (((mul) (e_l)) (e_l)))) (((mul) (e_l)) (e_l)),1.331808720488678E-4,((axiom_{eqM(mul(e_l)(a))(a)}) (((mul) (e_l)) (e_l)),7.160861095995352E-4)), (((eqM) (((mul) (((mul) (e_r)) (e_r))) (e_r))) (((mul) (e_r)) (e_r)),1.4205298218081784E-4,((axiom_{eqM(mul(a)(e_r))(a)}) (((mul) (e_r)) (e_r)),7.160861095995352E-4)), (((eqM) (((mul) (((mul) (e_l)) (e_r))) (e_r))) (((mul) (e_l)) (e_r)),1.5047987030559408E-4,((axiom_{eqM(mul(a)(e_r))(a)}) (((mul) (e_l)) (e_r)),7.160861095995352E-4)), (((eqM) (((mul) (e_r)) (e_r))) (((mul) (e_r)) (e_r)),0.00114548891681392,((axiom_{eqM(a)(a)}) (((mul) (e_r)) (e_r)),7.160861095995352E-4)), ((`a : M ) ~> (((eqM) (((mul) (e_l)) (`a))) (`a)),0.028571428571428577,(axiom_{eqM(mul(e_l)(a))(a)},0.028571428571428584)), (((eqM) (e_r)) (((mul) (e_r)) (e_r)),0.006178815922198932,((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_r)) (e_r))) (e_r)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_r)),3.0553118004127795E-4)), (((eqM) (((mul) (e_l)) (e_r))) (e_r),0.006210923361711237,((axiom_{eqM(mul(e_l)(a))(a)}) (e_r),0.0035726659386709513)), (((eqM) (((mul) (e_l)) (e_l))) (e_l),0.006210923361711237,((axiom_{eqM(mul(e_l)(a))(a)}) (e_l),0.0035726659386709513)), ((`a : M ) ~> ((`b : M ) ~> ((`c : M ) ~> ((((eqM) (`a)) (`b)) → ((((eqM) (`b)) (`c)) → (((eqM) (`a)) (`c)))))),0.028571428571428577,(axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))},0.028571428571428584)), (((eqM) (((mul) (e_l)) (e_l))) (((mul) (e_l)) (e_l)),0.0011454889168139202,((axiom_{eqM(a)(a)}) (((mul) (e_l)) (e_l)),7.160861095995352E-4)), (((eqM) (e_l)) (((mul) (e_l)) (e_r)),0.006178815922198932,((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l)),3.0553118004127795E-4)), (((eqM) (((mul) (e_l)) (((mul) (e_r)) (e_l)))) (((mul) (e_r)) (e_l)),1.331808720488678E-4,((axiom_{eqM(mul(e_l)(a))(a)}) (((mul) (e_r)) (e_l)),7.160861095995352E-4)), (((eqM) (((mul) (((mul) (e_l)) (e_r))) (e_l))) (((mul) (((mul) (e_l)) (e_r))) (e_l)),3.532658753698367E-5,((axiom_{eqM(a)(a)}) (((mul) (((mul) (e_l)) (e_r))) (e_l)),1.0125911493191799E-4)), (((eqM) (e_r)) (e_r),0.03351285781999383,((axiom_{eqM(a)(a)}) (e_r),0.0035726659386709513)), (((eqM) (((mul) (((mul) (e_r)) (e_l))) (e_l))) (((mul) (((mul) (e_r)) (e_l))) (e_l)),3.532658753698367E-5,((axiom_{eqM(a)(a)}) (((mul) (((mul) (e_r)) (e_l))) (e_l)),1.0125911493191799E-4)), (((eqM) (((mul) (((mul) (e_r)) (e_r))) (e_r))) (((mul) (((mul) (e_r)) (e_r))) (e_r)),3.42166952553264E-5,((axiom_{eqM(a)(a)}) (((mul) (((mul) (e_r)) (e_r))) (e_r)),9.036571162908673E-5)), (((eqM) (((mul) (e_l)) (((mul) (e_l)) (e_r)))) (((mul) (e_l)) (e_r)),1.331808720488678E-4,((axiom_{eqM(mul(e_l)(a))(a)}) (((mul) (e_l)) (e_r)),7.160861095995352E-4)), (((eqM) (((mul) (e_r)) (e_l))) (((mul) (e_r)) (e_l)),0.0011454889168139202,((axiom_{eqM(a)(a)}) (((mul) (e_r)) (e_l)),7.160861095995352E-4)), (((eqM) (e_l)) (e_l),0.03351285781999383,((axiom_{eqM(a)(a)}) (e_l),0.0035726659386709513)), (((eqM) (e_l)) (((mul) (e_l)) (e_l)),0.006178815922198932,((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_l))) (e_l)) ((axiom_{eqM(mul(e_l)(a))(a)}) (e_l)),3.0553118004127795E-4)), (((eqM) (((mul) (((mul) (e_l)) (e_l))) (e_r))) (((mul) (((mul) (e_l)) (e_l))) (e_r)),3.532658753698367E-5,((axiom_{eqM(a)(a)}) (((mul) (((mul) (e_l)) (e_l))) (e_r)),1.0125911493191799E-4)), (((eqM) (((mul) (((mul) (e_l)) (e_l))) (e_l))) (((mul) (((mul) (e_l)) (e_l))) (e_l)),3.532658753698367E-5,((axiom_{eqM(a)(a)}) (((mul) (((mul) (e_l)) (e_l))) (e_l)),1.0125911493191799E-4)), (((eqM) (((mul) (e_l)) (((mul) (e_r)) (e_r)))) (((mul) (e_r)) (e_r)),1.331808720488678E-4,((axiom_{eqM(mul(e_l)(a))(a)}) (((mul) (e_r)) (e_r)),7.160861095995352E-4)), (((eqM) (e_r)) (((mul) (e_l)) (e_r)),0.006178815922198932,((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_r)) ((axiom_{eqM(mul(e_l)(a))(a)}) (e_r)),3.0553118004127795E-4)), ((M) → ((M) → (𝒰 _0)),0.20000000000000004,(eqM,0.2000000000000001)), (((eqM) (((mul) (e_l)) (e_r))) (((mul) (e_l)) (e_r)),0.00114548891681392,((axiom_{eqM(a)(a)}) (((mul) (e_l)) (e_r)),7.160861095995352E-4)), (((eqM) (((mul) (((mul) (e_l)) (e_l))) (e_r))) (((mul) (e_l)) (e_l)),1.5047987030559413E-4,((axiom_{eqM(mul(a)(e_r))(a)}) (((mul) (e_l)) (e_l)),7.160861095995352E-4)), ((`a : M ) ~> (((eqM) (((mul) (`a)) (e_r))) (`a)),0.028571428571428577,(axiom_{eqM(mul(a)(e_r))(a)},0.028571428571428584)), ((M) → ((M) → (M)),0.20000000000000004,(mul,0.2000000000000001)), ((`a : M ) ~> ((`b : M ) ~> ((((eqM) (`a)) (`b)) → (((eqM) (`b)) (`a)))),0.028571428571428577,(axiom_{(eqM(a)(b) \to eqM(b)(a))},0.028571428571428584)), (((eqM) (((mul) (((mul) (e_r)) (e_l))) (e_r))) (((mul) (((mul) (e_r)) (e_l))) (e_r)),3.532658753698367E-5,((axiom_{eqM(a)(a)}) (((mul) (((mul) (e_r)) (e_l))) (e_r)),1.0125911493191799E-4)), (((eqM) (((mul) (e_r)) (e_r))) (e_r),0.006210923361711237,((axiom_{eqM(mul(a)(e_r))(a)}) (e_r),0.0035726659386709513)), (((eqM) (((mul) (e_l)) (e_r))) (e_l),0.006210923361711237,((axiom_{eqM(mul(a)(e_r))(a)}) (e_l),0.0035726659386709513)), (M,0.057142857142857155,(e_r,0.028571428571428584)))
println(thms.map(_._1).toVector.mkString("\n"))
// ((eqM) (((mul) (((mul) (e_l)) (e_r))) (e_r))) (((mul) (((mul) (e_l)) (e_r))) (e_r))
// ((eqM) (((mul) (((mul) (e_r)) (e_l))) (e_r))) (((mul) (e_r)) (e_l))
// ((eqM) (((mul) (((mul) (e_r)) (e_r))) (e_l))) (((mul) (((mul) (e_r)) (e_r))) (e_l))
// (`a : M ) ~> (((eqM) (`a)) (`a))
// ((eqM) (((mul) (e_l)) (((mul) (e_l)) (e_l)))) (((mul) (e_l)) (e_l))
// ((eqM) (((mul) (((mul) (e_r)) (e_r))) (e_r))) (((mul) (e_r)) (e_r))
// ((eqM) (((mul) (((mul) (e_l)) (e_r))) (e_r))) (((mul) (e_l)) (e_r))
// ((eqM) (((mul) (e_r)) (e_r))) (((mul) (e_r)) (e_r))
// (`a : M ) ~> (((eqM) (((mul) (e_l)) (`a))) (`a))
// ((eqM) (e_r)) (((mul) (e_r)) (e_r))
// ((eqM) (((mul) (e_l)) (e_r))) (e_r)
// ((eqM) (((mul) (e_l)) (e_l))) (e_l)
// (`a : M ) ~> ((`b : M ) ~> ((`c : M ) ~> ((((eqM) (`a)) (`b)) → ((((eqM) (`b)) (`c)) → (((eqM) (`a)) (`c))))))
// ((eqM) (((mul) (e_l)) (e_l))) (((mul) (e_l)) (e_l))
// ((eqM) (e_l)) (((mul) (e_l)) (e_r))
// ((eqM) (((mul) (e_l)) (((mul) (e_r)) (e_l)))) (((mul) (e_r)) (e_l))
// ((eqM) (((mul) (((mul) (e_l)) (e_r))) (e_l))) (((mul) (((mul) (e_l)) (e_r))) (e_l))
// ((eqM) (e_r)) (e_r)
// ((eqM) (((mul) (((mul) (e_r)) (e_l))) (e_l))) (((mul) (((mul) (e_r)) (e_l))) (e_l))
// ((eqM) (((mul) (((mul) (e_r)) (e_r))) (e_r))) (((mul) (((mul) (e_r)) (e_r))) (e_r))
// ((eqM) (((mul) (e_l)) (((mul) (e_l)) (e_r)))) (((mul) (e_l)) (e_r))
// ((eqM) (((mul) (e_r)) (e_l))) (((mul) (e_r)) (e_l))
// ((eqM) (e_l)) (e_l)
// ((eqM) (e_l)) (((mul) (e_l)) (e_l))
// ((eqM) (((mul) (((mul) (e_l)) (e_l))) (e_r))) (((mul) (((mul) (e_l)) (e_l))) (e_r))
// ((eqM) (((mul) (((mul) (e_l)) (e_l))) (e_l))) (((mul) (((mul) (e_l)) (e_l))) (e_l))
// ((eqM) (((mul) (e_l)) (((mul) (e_r)) (e_r)))) (((mul) (e_r)) (e_r))
// ((eqM) (e_r)) (((mul) (e_l)) (e_r))
// (M) → ((M) → (𝒰 _0))
// ((eqM) (((mul) (e_l)) (e_r))) (((mul) (e_l)) (e_r))
// ((eqM) (((mul) (((mul) (e_l)) (e_l))) (e_r))) (((mul) (e_l)) (e_l))
// (`a : M ) ~> (((eqM) (((mul) (`a)) (e_r))) (`a))
// (M) → ((M) → (M))
// (`a : M ) ~> ((`b : M ) ~> ((((eqM) (`a)) (`b)) → (((eqM) (`b)) (`a))))
// ((eqM) (((mul) (((mul) (e_r)) (e_l))) (e_r))) (((mul) (((mul) (e_r)) (e_l))) (e_r))
// ((eqM) (((mul) (e_r)) (e_r))) (e_r)
// ((eqM) (((mul) (e_l)) (e_r))) (e_l)
// M
println(thms.size)
// 38
val lem = eqM(l)(op(l)(r))
// lem: Typ[Term] = SymbTyp(
//   name = ApplnSym(
//     func = SymbolicFunc(
//       name = ApplnSym(
//         func = SymbolicFunc(
//           name = Name(name = "eqM"),
//           dom = SymbTyp(name = Name(name = "M"), level = 0),
//           codom = FuncTyp(dom = SymbTyp(name = Name(name = "M"), level = 0), codom = Universe(level = 0))
//         ),
//         arg = SymbObj(name = Name(name = "e_l"), typ = SymbTyp(name = Name(name = "M"), level = 0))
//       ),
//       dom = SymbTyp(name = Name(name = "M"), level = 0),
//       codom = Universe(level = 0)
//     ),
//     arg = SymbObj(
//       name = ApplnSym(
//         func = SymbolicFunc(
//           name = ApplnSym(
//             func = SymbolicFunc(
//               name = Name(name = "mul"),
//               dom = SymbTyp(name = Name(name = "M"), level = 0),
//               codom = FuncTyp(
//                 dom = SymbTyp(name = Name(name = "M"), level = 0),
//                 codom = SymbTyp(name = Name(name = "M"), level = 0)
//               )
//             ),
//             arg = SymbObj(name = Name(name = "e_l"), typ = SymbTyp(name = Name(name = "M"), level = 0))
//           ),
//           dom = SymbTyp(name = Name(name = "M"), level = 0),
//           codom = SymbTyp(name = Name(name = "M"), level = 0)
//         ),
//         arg = SymbObj(name = Name(name = "e_r"), typ = SymbTyp(name = Name(name = "M"), level = 0))
//       ),
//       typ = SymbTyp(name = Name(name = "M"), level = 0)
//     )
//   ),
//   level = 0
// )
val lemPf = thms.find(_._1 == lem).map(_._3).get._1
// lemPf: Term = SymbObj(
//   name = ApplnSym(
//     func = SymbolicFunc(
//       name = ApplnSym(
//         func = PiSymbolicFunc(
//           name = ApplnSym(
//             func = PiSymbolicFunc(
//               name = Name(name = "axiom_{(eqM(a)(b) \\to eqM(b)(a))}"),
//               variable = SymbObj(name = `a, typ = SymbTyp(name = Name(name = "M"), level = 0)),
//               value = PiDefn(
//                 variable = SymbObj(name = `b, typ = SymbTyp(name = Name(name = "M"), level = 0)),
//                 value = FuncTyp(
//                   dom = SymbTyp(
//                     name = ApplnSym(
//                       func = SymbolicFunc(
//                         name = ApplnSym(
//                           func = SymbolicFunc(
//                             name = Name(name = "eqM"),
//                             dom = SymbTyp(name = Name(name = "M"), level = 0),
//                             codom = FuncTyp(
//                               dom = SymbTyp(name = Name(name = "M"), level = 0),
//                               codom = Universe(level = 0)
//                             )
//                           ),
//                           arg = SymbObj(name = `a, typ = SymbTyp(name = Name(name = "M"), level = 0))
//                         ),
//                         dom = SymbTyp(name = Name(name = "M"), level = 0),
//                         codom = Universe(level = 0)
//                       ),
//                       arg = SymbObj(name = `b, typ = SymbTyp(name = Name(name = "M"), level = 0))
//                     ),
//                     level = 0
//                   ),
//                   codom = SymbTyp(
//                     name = ApplnSym(
//                       func = SymbolicFunc(
//                         name = ApplnSym(
//                           func = SymbolicFunc(
//                             name = Name(name = "eqM"),
//                             dom = SymbTyp(name = Name(name = "M"), level = 0),
//                             codom = FuncTyp(
//                               dom = SymbTyp(name = Name(name = "M"), level = 0),
//                               codom = Universe(level = 0)
//                             )
//                           ),
//                           arg = SymbObj(name = `b, typ = SymbTyp(name = Name(name = "M"), level = 0))
//                         ),
//                         dom = SymbTyp(name = Name(name = "M"), level = 0),
//                         codom = Universe(level = 0)
// ...
println(lemPf)
// (((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l))
val tpde = new ParTangentDistEq(ns.nodeCoeffSeq, nextState) 
// tpde: ParTangentDistEq = provingground.learning.ParTangentDistEq@7590d2ab
val tangentState = ParMapState(ParMap(lemPf -> 1.0), ParMap())
// tangentState: ParMapState = ParMapState(
//   termDist = ParHashMap((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l)) -> 1.0),
//   typDist = ParHashMap(),
//   vars = Vector(),
//   inds = ParHashMap(),
//   goalDist = ParHashMap(),
//   context = {}
// )
val (tangNextState, teqs) = tpde.nextStateEqs(tangentState, math.pow(10, -4))
// tangNextState: ParMapState = ParMapState(
//   termDist = ParMap(((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (((mul) (e_l)) (e_r))) (e_l)) (((mul) (((mul) (e_r)) (e_r))) (e_l))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (e_l)) (((mul) (e_l)) (e_r))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l)))) -> 1.3988872131648634E-4, ((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (e_l)) (e_r))) (((mul) (((mul) (e_l)) (e_l))) (e_l))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l))) -> 9.442852185677633E-4, ((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (e_l)) (e_r))) (((mul) (((mul) (((mul) (e_r)) (e_r))) (e_r))) (e_r))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l))) -> 1.8624385637016497E-4, ((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (e_l)) (e_r))) (((mul) (((mul) (e_r)) (((mul) (e_r)) (e_r)))) (e_l))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l))) -> 1.9667995928714332E-4, ((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (e_l)) (e_r))) (((mul) (e_r)) (((mul) (e_l)) (e_l)))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l))) -> 7.364298127703938E-4, ((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (e_l)) (e_r))) (((mul) (e_l)) (((mul) (e_r)) (e_l)))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l))) -> 7.364298127703938E-4, (`$angbtk :  M) ↦ (((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (e_r)) (`$angbtk)) ((((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (e_l)) (e_r))) (e_r)) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l)))) ((axiom_{eqM(mul(e_l)(a))(a)}) (e_r)))) -> 0.004243388637198068, ((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (e_l)) (e_r))) (((mul) (((mul) (((mul) (e_l)) (e_l))) (e_r))) (e_r))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l))) -> 1.8849165592253216E-4, (((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (e_l)) (e_r)) ((((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (e_l)) (e_r))) (e_r)) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l)))) ((axiom_{eqM(mul(e_l)(a))(a)}) (e_r))) -> 0.004243388637198068, ((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (e_l)) (e_r))) (((mul) (((mul) (e_l)) (((mul) (e_r)) (e_r)))) (e_r))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l))) -> 1.966799592871434E-4, ((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (e_l)) (e_r))) (((mul) (e_l)) (((mul) (((mul) (e_l)) (e_r))) (e_l)))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l))) -> 2.1385889674841707E-4, ((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (e_l)) (e_r))) (((mul) (((mul) (e_l)) (((mul) (e_l)) (e_l)))) (e_r))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l))) -> 1.9667995928714332E-4, ((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (e_l)) (e_r))) (((mul) (((mul) (((mul) (e_r)) (e_l))) (e_r))) (e_r))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l))) -> 1.8849165592253216E-4, ((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (e_l)) (e_r))) (((mul) (((mul) (e_l)) (((mul) (e_r)) (e_l)))) (e_r))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l))) -> 1.9667995928714332E-4, ((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (((mul) (e_l)) (e_r))) (e_l)) (((mul) (e_r)) (e_l))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (e_l)) (((mul) (e_l)) (e_r))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l)))) -> 5.441098120384744E-4, ((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (((mul) (e_l)) (e_r))) (e_l)) (((mul) (e_r)) (e_r))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (e_l)) (((mul) (e_l)) (e_r))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l)))) -> 5.441098120384743E-4, ((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (e_l)) (e_r))) (((mul) (((mul) (e_r)) (e_r))) (e_r))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l))) -> 9.442852185677633E-4, ((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (e_l)) (e_r))) (e_r)) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l))) -> 0.037610939620850575, ((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (e_l)) (e_r))) (((mul) (((mul) (((mul) (e_l)) (e_r))) (e_l))) (e_r))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l))) -> 1.884916559225322E-4, ((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (e_l)) (e_r))) (((mul) (e_r)) (((mul) (e_r)) (e_r)))) ((((axiom_{(eqM(a)(b) \to eqM(b)(a))}) (((mul) (e_l)) (e_r))) (e_l)) ((axiom_{eqM(mul(a)(e_r))(a)}) (e_l))) -> 7.364298127703938E-4, ((((axiom_{(eqM(a)(b) \to (eqM(b)(c) \to eqM(a)(c)))}) (e_l)) (((mul) (e_l)) (e_r))) (((mul) (((mul) (e_l)) (((mul) (e_l)) (e_r))...
// teqs: ParSet[EquationNode] = <function1>
val tThms = tangNextState.termDist.keySet.map(_.typ).intersect(nextState.typDist.keySet)
// tThms: ParSet[Typ[U] forSome { type U >: x$8 <: Term with Subs[U]; val x$8: Term }] = <function1>
tThms.contains(eqM(l)(r))
// res3: Boolean = true
