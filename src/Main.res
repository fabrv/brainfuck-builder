open Brainfuck

let (staticData, sdBf, totalSize) = buildStaticData([])

Console.log2("Static Data", staticData)

let instruction = [
  sdBf,
  loadSource(totalSize),
  push(48, ~staticSize=totalSize)
]->Array.join("\n")
Console.log(instruction)
