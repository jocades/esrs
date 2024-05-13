let start = time()
let max = 1000000 // 1 million

for _ in 0..max {}

echo("Range of", max, "took", time() - start, "ms")
