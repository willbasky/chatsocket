(** Initializes a new websocket server on the given URI. *)
val server : Uri.t -> unit Lwt.t
