let Class = {}

Class.__call = fn(name, age) {
  let obj = { 
    name,
    age,
    whoami: fn() {
      echo(this.name, this.age)
    },
    inc_age: fn() {
      this.age = this.age + 1
    },
  }

  obj
} 

let p1 = Class("John", 30)
p1.whoami() 
p1.inc_age()
p1.whoami() 

let p2 = Class("Jane", 25)
p2.whoami()
p2.inc_age()
p2.whoami()

