(** Initializes a new websocket client on the given URI. *)
val client : Uri.t -> unit Lwt.t
