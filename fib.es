fn fib(n) {
  if n < 2 {
    return n
  }

  fib(n - 1) + fib(n - 2)
}

let N = 30

let start = time()
let result = fib(N)
let elapsed = time() - start

echo("N:", N, "Result:", result, "Elapsed:", elapsed, "ms")
echo("N:", N, "Result:", result, "Elapsed:", elapsed / 1000, "sec")

