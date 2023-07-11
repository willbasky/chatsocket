open Chatsocket

let main is_server uri =
  if !is_server then (
    ignore @@ Server.server uri;
    fst @@ Lwt.wait ())
  else Client.client uri

let apply_loglevel = function
  | 2 -> Lwt_log.(add_rule "*" Info)
  | 3 -> Lwt_log.(add_rule "*" Debug)
  | _ -> ()

let () =
  let uri = ref "http://127.0.0.1:9001" in
  let server = ref false in
  let speclist =
    Arg.align
      [
        ("-s", Arg.Set server, " Run as server");
        ("-l", Arg.Int apply_loglevel, "1-3 Set loglevel");
      ]
  in
  let anon_fun s = uri := s in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> uri\nOptions are:" in
  Arg.parse speclist anon_fun usage_msg;
  Lwt_main.run (main server (Uri.of_string !uri))
