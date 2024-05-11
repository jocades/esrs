let mod = import ("mod.e")

echo("mod", mod)

echo("main", time())

for key in mod {
  if type(mod[key]) == "function" {
    echo("mod." + key, mod[key]())
  } else {
    echo("mod." + key, mod[key])
  }
}
