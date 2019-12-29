import org.scalatest.FunSuite

class Day9Test extends FunSuite {
	test("Day9.execute") {
		assert(IntCode2.execute(List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99).map(BigInt(_)), List()) === List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99).map(BigInt(_)))
		assert(IntCode2.execute(List(1102,34915192,34915192,7,4,7,99,0).map(BigInt(_)), List()) === List(BigInt("1219070632396864")))
		assert(IntCode2.execute(List(BigInt(104),BigInt("1125899906842624"),BigInt(99)), List()) === List(BigInt("1125899906842624")))
	}
}