let obj = {
  var: 0,
  get: fn() {
    echo("get", this.var)
    this.var
  },
  set: fn(value) {
    echo("set", value)
    this.var = value
  },
  is_big: fn () {
    echo("is_big", this.var)
    this.var > 10
  }
}

obj.get()
obj.set(4)
obj.get()

obj.set(20)
let result = obj.is_big()
echo(result)

fn what() {
  echo(this)
}

// what() => error

obj.what = what
obj.what() // => ok
