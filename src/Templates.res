let bootstrapper = `
# BRAINFUCK BOOTSTRAPPER
->+[
  >
  (A) Number of Registries
  +++
  Copy initial cell value
  <[>>+>+<<<-]
  >>>[<<<+>>>-]<<<
  Compare if it's (A)
  >[<->-]<[
  	Reinit index cell and create next index cell
  	[-]
  	>>[>+<<<+>>-]
    >+<
  ]
  >
]
>[<<+>>-]>-<<<
<+[
  -
  <<<+
]-
`
