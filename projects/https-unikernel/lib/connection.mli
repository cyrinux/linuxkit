(** Provides the RPC layer on top of [Endpoint]. *)

type t

type obj

type service = interface_id:Uint64.t -> method_id:int -> (module Sigs.METHOD)

val of_endpoint : ?offer:service -> ?tags:Logs.Tag.set -> Endpoint.t -> t
(** [of_endpoint ?offer endpoint] is fresh CapTP state for communicating with [endpoint].
    If [offer] is given, the peer can use the "Bootstrap" message to get access to it. *)

val bootstrap : t -> obj Lwt.t
(** [bootstrap t] is the peer's public bootstrap object, if any. *)

val call : obj -> interface:Uint64.t -> meth:int -> (Capnp.Message.rw Capnp.BytesMessage.Slice.t -> unit) -> Capnp.Message.ro Capnp.BytesMessage.Slice.t option Lwt.t
(** [call obj ~interface ~meth payload] creates a "Call" message, populates the payload with [payload], sends
    the message to the peer and returns the response. *)
