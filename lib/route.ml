type t =
  { _meth : Request.Method.t
  ; _path : string
  ; _handler : Handler.t
  }

let get path handler =
  { _meth = Request.Method.GET; _path = path; _handler = handler }
;;

let post path handler =
  { _meth = Request.Method.POST; _path = path; _handler = handler }
;;

let put path handler =
  { _meth = Request.Method.PUT; _path = path; _handler = handler }
;;

let delete path handler =
  { _meth = Request.Method.DELETE; _path = path; _handler = handler }
;;

let patch path handler =
  { _meth = Request.Method.PATCH; _path = path; _handler = handler }
;;

let options path handler =
  { _meth = Request.Method.OPTIONS; _path = path; _handler = handler }
;;
