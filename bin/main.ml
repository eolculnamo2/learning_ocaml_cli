open Glyph

let print_hello_world = Command.make 
    ~name:"print_hello"
    ~handler: (fun _ -> print_endline "hello command line")
    ()

let print_another_cmd = Command.make 
    ~name:"print2"
    ~handler: (fun args -> print_endline "print cmd 2")
    ~args: [Arg.make ~name:"foo"]
    ()

let () = App.run 
    ~name: "foo"
    ~summary: "my summary"
    ~commands: [print_hello_world; print_another_cmd]
    ()

