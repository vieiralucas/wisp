type t =
  { meth : Request.Method.t
  ; path : string
  ; handler : Handler.t
  }

let get path handler =
  { meth = Request.Method.GET; path = path; handler = handler }
;;

let post path handler =
  { meth = Request.Method.POST; path = path; handler = handler }
;;

let put path handler =
  { meth = Request.Method.PUT; path = path; handler = handler }
;;

let delete path handler =
  { meth = Request.Method.DELETE; path = path; handler = handler }
;;

let patch path handler =
  { meth = Request.Method.PATCH; path = path; handler = handler }
;;

let options path handler =
  { meth = Request.Method.OPTIONS; path = path; handler = handler }
;;

let match_request req routes =
  let rec aux = function
    | [] -> None
    | route :: rest ->
      if Request.meth req = route.meth && Request.path req = route.path
      then Some route.handler
      else aux rest
  in
  aux routes
;;
