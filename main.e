let mod = import ("mod.e")
let other = import ("other.e")

echo("mod", mod)
echo("other", other)

echo("mod.var", mod.var)

mod.var = 2
echo("mod.var", mod.var)

mod.get()
mod.set(3)
mod.get()
