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

fn add_bang(item, i) {
  echo(item, i)
  item + "!"
}

let new = ls.map(add_bang)
echo(new)

let users = []
for id in 0..10 {
  users.push({ id })
}

users.for_each(fn(user, i) {
  user.name = "user" + str(i)
})

echo(users)
