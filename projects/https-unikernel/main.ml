open Lwt.Infix
open Astring
open CapTP

module Reporter = Mirage_logs.Make(Pclock)
module Proto = Proto.Make(Capnp.BytesMessage)

let store_to_http, http_to_store = Unix.(socketpair PF_UNIX SOCK_STREAM 0)

let failf fmt = Fmt.kstrf failwith fmt

let endpoint_tag = Logs.Tag.def "endpoint" Fmt.string

module Store = struct
  let src = Logs.Src.create "web.store" ~doc:"Datastore for web server"
  module Log = (val Logs.src_log src: Logs.LOG)

  let get = function
    | ["index.html"] -> Some "The index"
    | _ -> None

  module Get : Sigs.METHOD = struct
    module Params = Proto.Reader.Store.Get.Params
    module Results = Proto.Builder.Store.Get.Results

    let handle req (resp:Results.t) =
      let path = Params.path_get_list req in
      Log.info (fun f -> f "Handing request for %a" (Fmt.Dump.list String.dump) path);
      let open Proto.Builder in
      let result = Results.result_init resp in
      begin match get path with
      | Some data -> Response.ok_set result data
      | None -> Response.not_found_set result
      end;
      Lwt.return ()
  end

  let service ~interface_id ~method_id =
    Log.info (fun f -> f "call %a:%d" Uint64.printer_hex interface_id method_id);
    match method_id with
    | 0 -> (module Get : Sigs.METHOD)
    | x -> failf "Unknown method %d" x

  let init () =
    let tags = Logs.Tag.add endpoint_tag "Store" Logs.Tag.empty in
    ignore (Connection.of_endpoint ~offer:service ~tags @@ Endpoint.of_socket store_to_http)
end

module HTTP_server = struct
(*
  let src = Logs.Src.create "web.http" ~doc:"HTTP engine for web server"
  module Log = (val Logs.src_log src: Logs.LOG)
*)

  module Server = Cohttp_lwt.Make_server(Cohttp_lwt_unix_io)

  type t = {
    store : Connection.obj;
  }

  let connect () =
    let endpoint = Endpoint.of_socket http_to_store in
    let tags = Logs.Tag.add endpoint_tag "Server" Logs.Tag.empty in
    Connection.bootstrap (Connection.of_endpoint ~tags endpoint) >>= fun store ->
    Lwt.return { store }

  let get t path =
    begin
      let open Proto.Builder in
      Connection.call t.store ~interface:Store.interface_id ~meth:Store.Get.method_id (fun ptr ->
          let open Store.Get in
          let p = Params.init_pointer ptr in
          ignore (Params.path_set_list p path)
        )
    end >>= fun ptr ->
    let open Proto.Reader in
    let msg = Store.Get.Results.of_pointer ptr in
    match Response.get (Store.Get.Results.result_get msg) with
    | Response.NotFound -> Lwt.return None
    | Response.Ok data -> Lwt.return (Some data)
    | Response.Undefined _ -> failwith "Protocol error: bad msg type"

  let callback t _conn req _body =
    let open Cohttp in
    let uri = Request.uri req in
    match Request.meth req with
    | `GET ->
      let path = String.cuts ~empty:false ~sep:"/" (Uri.path uri) in
      let path =
        match path with
        | [] -> ["index.html"]
        | p -> p
      in
      begin get t path >>= function
      | Some body -> Server.respond_string ~status:`OK ~body ()
      | None -> Server.respond_not_found ~uri ()
      end
    | m ->
      let body = Fmt.strf "Bad method %S" (Code.string_of_method m) in
      Server.respond_error ~status:`Bad_request ~body ()

  let callback t = Server.callback (Server.make ~callback:(callback t) ())
end

module TLS = struct
  let mode = `TCP (`Port 8000)

  let listen http =
    Conduit_lwt_unix.(serve ~ctx:default_ctx) ~mode (fun flow ic oc ->
        HTTP_server.callback http flow ic oc
      )
end

let () =
  Lwt_main.run begin
    Pclock.connect () >|= Reporter.create >|= Reporter.set_reporter >>= fun () ->
    Logs.set_level (Some Logs.Info);
    Store.init ();
    HTTP_server.connect () >>= TLS.listen
  end
