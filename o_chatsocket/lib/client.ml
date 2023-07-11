open Lwt.Infix
open Websocket_lwt_unix
module A = Astring
open Common

let rec react conn trip () =
    let open Websocket in
    (* The logic of ping, pong and text opcodes is similar to server's one *)
    Websocket_lwt_unix.read conn >>= function
    | { Frame.opcode = Ping; _ } ->
          write conn (Frame.create ~opcode:Pong ()) >>=
          react conn trip
    | { opcode = Pong; _ } ->
          Lwt_io.print "Server: pong\n" >>= fun () ->
          react conn trip ()
    | { opcode = Close; content; _ } ->
        (if String.length content >= 2
          then
            let frame = Frame.create ~opcode:Close ~content:(String.sub content 0 2) () in
            write conn frame
          else write conn @@ Frame.close 1000)
        >>= fun () -> Websocket_lwt_unix.close_transport conn
    | { opcode = Text; content; _ } | { opcode = Binary; content; _ } ->
          let end_time = Unix.gettimeofday () in
          let with_acknowledge = String.starts_with ~prefix:acknowledge content in
          if with_acknowledge
            then trip_finish content trip end_time >>= react conn trip
            else
              let trip_message = acknowledge ^ content in
              write conn (Frame.create ~content:trip_message ()) >>= fun () ->
              Lwt_io.printf "Server: %s\n" content >>= react conn trip
    | _ -> Websocket_lwt_unix.close_transport conn

(* The logic of pushf is similar to server's one,
but without pointing the address
*)
let rec pushf conn trip () =
    let open Websocket in
    Lwt_io.(read_line_opt stdin) >>= function
    | None -> pushf conn trip ()
    | Some cont ->
      match cont with
      | "#ping" -> write conn (Frame.create ~opcode:Ping ()) >>= react conn trip
      | "#close" -> write conn @@ Frame.close 1000
      | _ ->
        let key = take 10 cont in
        write conn (Frame.create ~content:cont ()) >>= fun () ->
        let start_time = Unix.gettimeofday () in
        trip_start trip key start_time
        >>= pushf conn trip

let client uri =
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  Conduit_lwt_unix.endp_to_client ~ctx endp >>= fun client ->
  connect ~ctx client uri >>= fun conn ->
  (* The client has its own rountrip database *)
  let trip = Lwt_mvar.create BatMap.empty in
  pushf conn trip () <?> react conn trip ()
