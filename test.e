// Literal
1
"Hello, World!"
true
false
nil

// BinaryOp
2 + 2 * 2
(2 + 2) * 2

// Let
let x = 1

// Variable
x

// Block
let a = "outer"
{
  a

  let b = "inner"
  b

  let a = "shadow"
  a
}

// While
let i = 0
while i < 3 {
  i = i + 1
}
i

// Function
fn main() {
  1
}
main()
