// Literal
1
"Hello, World!"
true
false
nil

// BinaryOp
echo(2 + 2 * 2)
echo((2 + 2) * 2)

// Let
let x = 1

// Variable
echo(x)

// Assign
x = 2
echo(x)

// Block
let a = "outer"
{
  echo(a)

  let b = "inner"
  echo(b)

  let a = "shadow"
  echo(a)
}

// While
let i = 0
while i < 3 {
  echo("while", i)
  i = i + 1
}

// Function
fn main() {
  let x = "outer"

  fn inner() {
    echo("inner")
    echo(x)
  }

  inner()
}

main()

// List
let list = ["a", "b", "c"]
echo(list)
//echo(list[0])
//echo(list[1])
//
//
//// For
//for i in list {
//  echo("for", i)
//}
//
//for i in 0..3 {
//  echo("for", i)
//}
