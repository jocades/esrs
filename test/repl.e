let io = import("std/io")

let state = { count: 0 }

let commands = {
  inc: fn() { state.count = state.count + 1 },
  dec: fn() { state.count = state.count - 1 }
}

loop {
  let command = io.input("Enter a command: ")
  echo("The count is now:", state.count)
}
