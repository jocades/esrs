let io = import("std/io")

fn fib(n) {
  if n < 2 {
     return n
  } 

  fib(n - 1) + fib(n - 2)
}

fn main() {
  loop {
    let n = num(io.input("enter a number: "))
    if n <= 0 {
      echo("Goodbye!")
      break
    }
    let start = time()
    let result = fib(n)
    echo("The result is: ", result)
    let elapsed = time() - start
    echo("Took:", elapsed, "ms")
    echo("Took:", elapsed / 1000, "sec")
  }
}

main()

