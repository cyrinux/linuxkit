module type METHOD = sig
  module Params : sig
    type t

    val of_pointer : Capnp.Message.ro Capnp.BytesMessage.Slice.t option -> t
  end

  module Results : sig
    type t

    val init_pointer : Capnp.Message.rw Capnp.BytesMessage.Slice.t -> t
  end

  val handle : Params.t -> Results.t -> unit Lwt.t
end
