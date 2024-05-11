let io = import("std/io")

fn fib(n) {
  if n < 2 {
     n
  } else {
    fib(n - 1) + fib(n - 2)
  }
}

fn main() {
  loop {
    let n = num(io.input("enter a number: "))
    if n <= 0 {
      echo("Goodbye!")
      break
    }
    let result = fib(n)
    echo("The result is: ", result)
  }
}

main()

