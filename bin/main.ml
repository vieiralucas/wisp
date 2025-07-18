let handle_list_notes _req = Wisp.Response.text "TODO: List notes"
let handle_create_note _req = Wisp.Response.text "TODO: Create note"
let handle_get_note _req = Wisp.Response.text "TODO: Get note"
let handle_update_note _req = Wisp.Response.text "TODO: Update note"
let handle_delete_note _req = Wisp.Response.text "TODO: Delete note"

let main env sw =
  let net = Eio.Stdenv.net env in
  Wisp.Server.listen
    ~net
    ~sw
    ~port:8080
    [ Wisp.Route.get "/notes" handle_list_notes
    ; Wisp.Route.post "/notes" handle_create_note
    ; Wisp.Route.get "/notes/:id" handle_get_note
    ; Wisp.Route.put "/notes/:id" handle_update_note
    ; Wisp.Route.delete "/notes/:id" handle_delete_note
    ]
;;

let () = Eio_main.run @@ fun env -> Eio.Switch.run @@ fun sw -> main env sw
