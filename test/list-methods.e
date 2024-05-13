let ls = []

for char in "abc" {
  ls.push(char)
}

echo(ls)
echo(len(ls))
ls.pop()
echo(ls)

let new = ls.map(fn(item, _) { echo(item, _) item + "!" })
echo(new)
