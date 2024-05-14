let io = import("std/io")

let state = { count: 0 }

let commands = {
  get: fn(_) { echo(state) },
  inc: fn(val) {
    if val { state.count = state.count + val }
    else { state.count = state.count + 1 }
  },
  dec: fn(val) { 
    if val {
      state.count = state.count - val
    } else {
      state.count = state.count - 1 
    }
  }
}

loop {
  let input = io.input("Enter a command: ")

  if input == "exit" {
    break
  }

  let parts = input.split()
  echo(parts)

  let cmd, val

  if len(parts) == 1 {
    cmd = parts[0]
  } else {
    cmd = parts[0]
    val = parts[1]

    if !val.is_number() {
      echo("Invalid number")
      break 
    }

    val = num(val)
  }

  if len(parts) == 1 {
    cmd = parts[0]
  }

  if len(parts) == 1 then
    cmd = parts[]
  end

  fn main() {
    echo("Hello World!")
  }

  fn main()
    echo("Hello World!")
  end

  for i in 0..10 {
    ...
  }

  for i in 0..10 do
    ...
  end

  loop {
    echo("loop")
    break
  }

  loop
    echo("loop")
    break
  end


  if !commands[cmd] {
    echo("Command not found")
  } else {
    commands[cmd](val)
  }
}
