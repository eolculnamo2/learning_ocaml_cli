module Arg = struct
    type t = {
        name: string;
        short: string option;
    }

    let make ~(name:string) ?(short=None) () = { 
        name;
        short;
    }
end

module Command = struct
    type t  = {
        name: string;
        args: Arg.t list;
        handler: (string list -> unit);
    }
    
    let make 
    ~(name:string)
    ?(args: Arg.t list = [])
    ~(handler: ('a -> unit))
    ()
    = {
        name;
        args;
        handler;
    }
end

exception CommandNotFound of string
exception InvariantViolation of string

module App = struct

    type program_details = {
        name: string;
        summary: string;
        version: string;
    }

    (* educational note: this is also valid *)
    (* let command_names = commands |> List.map(fun c -> Command.(c.name)) in *)
    let get_command_names commands =  commands |> List.map(fun c -> c.Command.name) 

    let get_handler commands cmd = match (commands |> List.find_opt (fun c -> c.Command.name = cmd)) with                                                                                 
           | None -> raise (CommandNotFound ("Command from list not found " ^ cmd))
           | Some c -> c.Command.handler 

    let parse_and_get_handler args commands details = match args with 
        | [] | ["--help"] -> 
            (fun () -> print_endline @@ "Verion: " ^ details.version ^  " Name:" ^ details.name ^ " , Summary: " ^ details.summary)
        | cmd :: _ when get_command_names(commands) |> List.mem cmd -> get_handler(commands cmd)
        | cmd :: _ -> raise @@ CommandNotFound cmd

    let run 
    ~(name:string)
    ?(summary="")
    ?(version="")
    ~(commands: Command.t list) 
    () =
      let args = Array.to_list Sys.argv |> List.tl in
      parse_and_get_handler args commands {
          name;
          summary;
          version;
      }
end

