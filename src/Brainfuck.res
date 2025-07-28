type bf = string
type bfInt = string
type register = int
type operand = Register(register) | Value(int)

let bfInt = (val: int): bfInt => {
  if val < 128 {
    "+"->String.repeat(val->mod(256))
  } else {
    "-"->String.repeat((256 - val)->mod(256))
  }
}

let toCharCodes = (val: string): array<int> =>
  val->String.split("")->Array.map(char => char->String.charCodeAt(0)->Float.toInt)

let repeat = String.repeat
let operation = (op: string) => (val: int): bf => {
  op->String.repeat(val)
}

let forward = operation(">")
let backward = operation("<")
let increment = operation("+")
let decrement = operation("-")
let print = "."
let input = ","
let bracketLeft = "["
let bracketRight = "]"

let clearCell = "[-]"

let toCell = value => forward(17 + value)
let fromCell = value => backward(17 + value)

let cellToBinary = "<[-]>[<+>[-]]<[->+<]>"

type staticDataValue = {location: int, size: int}
type staticData = dict<staticDataValue>
type regions = array<(string, array<int>)>

let buildStaticData = (regions: regions) => {
  let totalSize = regions->Array.reduce(0, (acc, (_k, value)) => {
    acc + value->Array.length
  })
  let locations = regions->Array.reduceWithIndex([], (acc, (_k, value), index) => {
    let location = acc->Array.get(index)->Option.getOr(0) + value->Array.length + 1
    [...acc, location]
  })

  let data: staticData =
    regions
    ->Array.mapWithIndex(((keyword, val), index) => {
      let location = locations->Array.get(index - 1)->Option.getOr(0)
      let dataValue: staticDataValue = {
        location,
        size: val->Array.length,
      }
      (keyword, dataValue)
    })
    ->Dict.fromArray

  let brainfuck =
    regions
    ->Array.map(((_k, value)) => value->Array.map(bfInt)->Array.join(">"))
    ->Array.join(">")

  (data, `${forward(32)}${brainfuck}${backward(31 + totalSize)}`, totalSize)
}

let mov = (operand: operand, ~register: register): bf => {
  switch operand {
  | Value(value) =>
    [
      forward(3),
      forward(3)->repeat(register),
      clearCell,
      bfInt(value),
      backward(3)->repeat(register),
      backward(3),
    ]->Array.join("")
  | Register(opReg) =>
    [
      forward(3),
      forward(3)->repeat(register),
      clearCell,
      backward(3)->repeat(register),
      forward(3)->repeat(opReg),
      bracketLeft,
      backward(1),
      increment(1),
      backward(3)->repeat(opReg),
      forward(1),
      forward(3)->repeat(register),
      increment(1),
      backward(3)->repeat(register),
      forward(3)->repeat(opReg),
      decrement(1),
      bracketRight,
      backward(1),
      bracketLeft,
      forward(1),
      increment(1),
      backward(1),
      decrement(1),
      bracketRight,
      backward(3)->repeat(opReg),
      backward(2),
    ]->Array.join("")
  }
}

let add = (operand: operand): bf => {
  switch operand {
  | Value(value) => [forward(3), "+"->repeat(value), backward(3)]->Array.join("")
  | Register(opReg) =>
    [
      forward(3),
      forward(3)->repeat(opReg),
      bracketLeft,
      backward(1),
      increment(1),
      backward(3)->repeat(opReg),
      forward(1),
      increment(1),
      forward(3)->repeat(opReg),
      decrement(1),
      bracketRight,
      backward(1),
      bracketLeft,
      forward(1),
      increment(1),
      backward(1),
      decrement(1),
      bracketRight,
      backward(3)->repeat(opReg),
      backward(2),
    ]->Array.join("")
  }
}

let sub = (operand: operand): bf => {
  switch operand {
  | Value(value) => [forward(3), "-"->repeat(value), backward(3)]->Array.join("")
  | Register(opReg) =>
    [
      forward(3),
      forward(3)->repeat(opReg),
      bracketLeft,
      backward(1),
      increment(1),
      backward(3)->repeat(opReg),
      forward(1),
      decrement(1),
      forward(3)->repeat(opReg),
      decrement(1),
      bracketRight,
      backward(1),
      bracketLeft,
      forward(1),
      increment(1),
      backward(1),
      decrement(1),
      bracketRight,
      backward(3)->repeat(opReg),
      backward(2),
    ]->Array.join("")
  }
}

let mul = (operand: operand): bf => {
  switch operand {
  | Value(value) =>
    [
      forward(2),
      clearCell,
      forward(1),
      bracketLeft,
      backward(1),
      increment(1),
      forward(1),
      decrement(1),
      bracketRight,
      backward(1),
      bracketLeft,
      forward(1),
      increment(value),
      backward(1),
      decrement(1),
      bracketRight,
      backward(2),
    ]->Array.join("")
  | Register(opReg) =>
    [
      forward(2),
      clearCell,
      forward(3)->repeat(opReg),
      clearCell,
      backward(3)->repeat(opReg),
      forward(1),
      bracketLeft,
      forward(3)->repeat(opReg),
      backward(1),
      increment(1),
      backward(3)->repeat(opReg),
      forward(1),
      decrement(1),
      bracketRight,
      forward(3)->repeat(opReg),
      backward(1),
      bracketLeft,
      forward(1),
      bracketLeft,
      backward(3)->repeat(opReg),
      increment(1),
      backward(1),
      increment(1),
      forward(3)->repeat(opReg),
      forward(1),
      decrement(1),
      bracketRight,
      backward(3)->repeat(opReg),
      backward(1),
      bracketLeft,
      forward(3)->repeat(opReg),
      forward(1),
      increment(1),
      backward(3)->repeat(opReg),
      backward(1),
      decrement(1),
      bracketRight,
      forward(3)->repeat(opReg),
      decrement(1),
      bracketRight,
      backward(3)->repeat(opReg),
      backward(2),
    ]->Array.join("")
  }
}

let _and = (operand: operand): bf => {
  switch operand {
  | Value(value) => [forward(3), value == 0 ? "[-]" : cellToBinary, backward(3)]->Array.join("")
  | Register(opReg) =>
    [
      toCell(0),
      clearCell,
      forward(1),
      clearCell,
      fromCell(1),
      forward(3)->repeat(opReg),
      forward(3),
      bracketLeft,
      backward(3)->repeat(opReg),
      backward(3),
      toCell(0),
      increment(1),
      forward(1),
      increment(1),
      fromCell(1),
      forward(3)->repeat(opReg),
      forward(3),
      decrement(1),
      bracketRight,
      backward(3)->repeat(opReg),
      backward(3),
      toCell(1),
      bracketLeft,
      fromCell(1),
      forward(3)->repeat(opReg),
      forward(3),
      increment(1),
      backward(3)->repeat(opReg),
      backward(3),
      toCell(1),
      decrement(1),
      bracketRight,
      fromCell(1),
      forward(3),
      cellToBinary,
      forward(3)->repeat(opReg),
      cellToBinary,
      backward(4),
      clearCell,
      forward(1),
      bracketLeft,
      forward(3)->repeat(opReg),
      bracketLeft,
      backward(3)->repeat(opReg),
      backward(1),
      increment(1),
      forward(1),
      forward(3)->repeat(opReg),
      decrement(1),
      bracketRight,
      backward(3)->repeat(opReg),
      decrement(1),
      bracketRight,
      forward(3)->repeat(opReg),
      clearCell,
      backward(3)->repeat(opReg),
      backward(3),
      toCell(0),
      bracketLeft,
      fromCell(0),
      forward(3)->repeat(opReg),
      forward(3),
      increment(1),
      backward(3)->repeat(opReg),
      backward(3),
      toCell(0),
      decrement(1),
      bracketRight,
      fromCell(0),
      ">>[>+<-]<<",
    ]->Array.join("")
  }
}
