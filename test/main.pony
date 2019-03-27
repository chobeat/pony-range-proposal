use "ponytest"
use "../range-proposal"

actor Main is TestList
 new create(env: Env) => PonyTest(env, this)

  new make() => None

  fun tag tests(test: PonyTest) =>
    test(RangeTests)
    //test(RangeOverflowTests)
    test(RangeErrorTests)
    test(BoundTest)