module A = Astring
open Lwt.Infix

let section = Lwt_log.Section.make "ahrefs"

let modify_mvar_ m f =
  Lwt_mvar.take m >>= fun a ->
  f a >>= fun newValue ->
  Lwt_mvar.put m newValue

let read_mvar m =
  Lwt_mvar.take m >>= fun a ->
  Lwt_mvar.put m a >>= fun () ->
  Lwt.return a

let take n s =
  let len = String.length s in
  String.sub s 0 (if len >= n then n else len)

let acknowledge = "Message received: "

let extract_key str =
  take 10
   @@ A.String.drop ~max:2
   @@ A.String.drop ~sat:(fun a -> a != ':')
   str

let trip_finish content trip end_time =
  let trip_key = extract_key content in
  modify_mvar_ trip (fun (a : (string,float) BatMap.t) ->
    let start_time = BatMap.find_opt trip_key a in
    match start_time with
      | None ->
          Lwt_io.print "Start_time wasn't found" >>= fun () ->
          Lwt.return a
      | Some st ->
        Lwt_io.printf "Roundtrip for message for %s is %.12f\n"
          trip_key
          (end_time -. st) >>= fun () ->
            Lwt.return @@ BatMap.remove trip_key a)

let trip_start trip key start_time =
  modify_mvar_ trip (fun a -> Lwt.return @@ BatMap.add key start_time a)

let get_client str =
  let (id, str) = A.String.span ~sat:BatChar.is_digit str in
  (Stdlib.int_of_string_opt id, str)

