open Lwt.Infix
open Websocket_lwt_unix
open Common

let section = Lwt_log.Section.make "ahrefs"

(*
Server is able to handle several opcodes from clients
*)
let rec react trip client client_id =
  let open Websocket in
  Connected_client.recv client >>= fun fr ->
  Lwt_log.debug_f ~section "Client %d: %S" client_id Frame.(show fr)
  >>= fun () ->
  match fr.opcode with
  | Frame.Opcode.Ping ->
      let frame = Frame.(create ~opcode:Opcode.Pong ~content:fr.content ()) in
      Connected_client.send client frame >>= fun () ->
      react trip client client_id
  | Close ->
      if String.length fr.content >= 2 then
        let content = String.sub fr.content 0 2 in
        let frame = Frame.(create ~opcode:Opcode.Close ~content ()) in
        Connected_client.send client frame
      else Connected_client.send client @@ Frame.close 1000
  | Pong ->
      Lwt_io.printf "Client %d: pong\n" client_id >>= fun () ->
      react trip client client_id

  | Text | Binary ->
    let end_time = Unix.gettimeofday () in
    let content = fr.content in
    let with_acknowledge = String.starts_with ~prefix:acknowledge content in
    (* Acknoledge messages is handled with counting roundtrip *)
    if with_acknowledge
      then trip_finish content trip end_time >>= fun () -> react trip client client_id
      else
        (* before printing received message server sends acknoladge to client
           Client does the same.
        *)
        let trip_message = acknowledge ^ content in
        Connected_client.send client (Frame.create ~content:trip_message ()) >>= fun () ->
        Lwt_io.printf "Client %d: %s\n" client_id content >>= fun () ->
        react trip client client_id

  | _ -> Connected_client.send client Frame.(close 1002)

(* Server send messages to specific client by its id.
Example of message:
1hello sends hello to client 1
There are two commands: ping and close
0#ping return pong of the client 0
0#close send close command to client 0
*)
let rec pushf clients trip =
  read_mvar clients >>= fun cs ->
  let open Websocket in
  Lwt_io.(read_line_opt stdin) >>= function
  | None -> pushf clients trip
  | Some cont ->
      let (id_opt, str) = get_client cont in
      match Option.bind id_opt (fun id -> Hashtbl.find_opt cs id) with
      | None ->
          Lwt_io.print "Invalid client id passed\n" >>= fun () ->
          pushf clients trip
      | Some client ->
          match str with
          | "#ping" ->
              Connected_client.send client (Frame.create ~opcode:Ping ()) >>= fun () ->
              pushf clients trip
          | "#close" ->
              Connected_client.send client @@ Frame.close 1000
          | _ ->
            let key = take 10 str in
            Connected_client.send client (Frame.create ~content:str ()) >>= fun () ->
            let start_time = Unix.gettimeofday () in
            trip_start trip key start_time >>= fun () ->
            pushf clients trip


let server uri =
  let id = ref (-1) in
  (* Map of start times of ever sent messages.
     When roundtrip finished the key*value is removed *)
  let trip = Lwt_mvar.create @@ BatMap.empty in
  (* Map holds pairs id/client it is address book for server *)
  let clients = Lwt_mvar.create @@ (Hashtbl.create 0 : (int, Connected_client.t) Hashtbl.t) in
  let echo_fun client =
    incr id;
    let id = !id in
    modify_mvar_ clients (fun cs -> Hashtbl.add cs id client; Lwt.return cs) >>= fun () ->
    Lwt_log.info_f ~section "Connection from client id %d" id >>= fun () ->
    Lwt.catch
      (fun () -> react trip client id <?> pushf clients trip)
      (fun exn ->
        Lwt_log.error_f ~section ~exn "Client %d error" id >>= fun () ->
        Lwt.fail exn)
  in
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
  let open Conduit_lwt_unix in
  let endp_str = endp |> Conduit.sexp_of_endp |> Sexplib.Sexp.to_string_hum in
  Lwt_log.info_f ~section "endp = %s" endp_str >>= fun () ->
  let ctx = Lazy.force default_ctx in
  endp_to_server ~ctx endp >>= fun server ->
  let server_str = server |> sexp_of_server |> Sexplib.Sexp.to_string_hum in
  Lwt_log.info_f ~section "server = %s" server_str >>= fun () ->
  establish_server ~ctx ~mode:server echo_fun
