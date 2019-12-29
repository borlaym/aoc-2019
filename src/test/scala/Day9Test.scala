import org.scalatest.FunSuite

class Day9Test extends FunSuite {
	test("Day9.execute") {
		assert(IntCode2.execute(List(109, -1, 4, 1, 99).map(BigInt(_)), List()) === List(-1).map(BigInt(_)))
		assert(IntCode2.execute(List(109, -1, 104, 1, 99).map(BigInt(_)), List()) === List(1).map(BigInt(_)))
		assert(IntCode2.execute(List(109, -1, 204, 1, 99).map(BigInt(_)), List()) === List(109).map(BigInt(_)))
		assert(IntCode2.execute(List(109, 1, 9, 2, 204, -6, 99).map(BigInt(_)), List()) === List(204).map(BigInt(_)))
		assert(IntCode2.execute(List(109, 1, 109, 9, 204, -6, 99).map(BigInt(_)), List()) === List(204).map(BigInt(_)))
		assert(IntCode2.execute(List(109, 1, 209, -1, 204, -106, 99).map(BigInt(_)), List()) === List(204).map(BigInt(_)))
		assert(IntCode2.execute(List(109, 1, 3, 3, 204, 2, 99).map(BigInt(_)), List(BigInt(8))) === List(8).map(BigInt(_)))
		assert(IntCode2.execute(List(109, 1, 203, 2, 204, 2, 99).map(BigInt(_)), List(BigInt(8))) === List(8).map(BigInt(_)))
//		assert(IntCode2.execute(List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99).map(BigInt(_)), List()) === List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99).map(BigInt(_)))
		assert(IntCode2.execute(List(1102,34915192,34915192,7,4,7,99,0).map(BigInt(_)), List()) === List(BigInt("1219070632396864")))
//		assert(IntCode2.execute(List(BigInt(104),BigInt("1125899906842624"),BigInt(99)), List()) === List(BigInt("1125899906842624")))
	}
}