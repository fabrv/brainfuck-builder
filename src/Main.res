open Brainfuck

let (staticData, sdBf, totalSize) = buildStaticData([
  ("var1", [21]),
  ("myStr", "hello"->toCharCodes),
  ("name", "Mary!"->toCharCodes)
])

let instruction = [
  sdBf,
  Value(20)->mov(~register=1),
  Value(1)->mov(~register=0),
  Register(1)->_and
]->Array.join("\n")
Console.log(instruction)
