# Wisp ✨

> A toy HTTP framework built from scratch in OCaml

**Wisp** is an experimental, incomplete HTTP framework that started as a learning project to explore OCaml, Eio, and the fundamentals of web servers. It's intentionally minimal and serves as a playground for understanding how HTTP frameworks work under the hood.

## 🎯 What is this?

This project represents my journey learning OCaml and exploring modern OCaml development:

1. **Learning OCaml** - Getting familiar with the language and its ecosystem
2. **Exploring Eio** - Understanding OCaml's modern I/O library
3. **Building a Web API** - Creating HTTP endpoints and handling requests
4. **HTTP Server from Scratch** - Implementing a basic HTTP server without external dependencies
5. **Parser Combinators** - Building a custom parser to handle HTTP requests

## 🚀 Features

- **Minimal HTTP Server** - Basic HTTP/1.1 server implementation
- **Route Matching** - Simple routing with path parameters (`/notes/:id`)
- **HTTP Method Support** - GET, POST, PUT, DELETE, PATCH, OPTIONS
- **Custom Parser Combinators** - Hand-written HTTP request parser
- **Eio Integration** - Built on OCaml's modern async I/O library

## 📦 Installation

```bash
# Clone the repository
git clone https://github.com/vieiralucas/wisp.git
cd wisp

# Install dependencies (requires opam)
opam install . --deps-only

# Build the project
dune build

# Run example notes api
dune exec -- notes
```

## 🛠️ Usage

```ocaml
open Eio_main

(* Simple handler functions *)
let handle_list_notes _req = Wisp.Response.text "TODO: List notes"
let handle_create_note _req = Wisp.Response.text "TODO: Create note"
let handle_get_note _req = Wisp.Response.text "TODO: Get note"
let handle_update_note _req = Wisp.Response.text "TODO: Update note"
let handle_delete_note _req = Wisp.Response.text "TODO: Delete note"

(* Main server function *)
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

(* Entry point *)
let () = run @@ fun env -> Eio.Switch.run @@ fun sw -> main env sw
```

### Example API Endpoints

```bash
# List all notes
curl http://localhost:8080/notes

# Get a specific note
curl http://localhost:8080/notes/123

# Create a new note
curl -X POST http://localhost:8080/notes

# Update a note
curl -X PUT http://localhost:8080/notes/123

# Delete a note
curl -X DELETE http://localhost:8080/notes/123
```

## 🏗️ Architecture

The framework is organized into several modules:

- **`Server`** - Main server implementation using Eio
- **`Route`** - Route matching and HTTP method handling
- **`Request`** - HTTP request parsing and representation
- **`Response`** - HTTP response generation
- **`Parser`** - Custom parser combinators for HTTP parsing
- **`Handler`** - Request handler functions

## 🧪 Testing

Run the test suite:

```bash
dune runtest
```

The project includes tests for:

- Parser combinators
- HTTP request parsing
- Route matching

## 🎓 Learning Goals

This project was built to learn:

- **OCaml Language Features** - Pattern matching, modules, functors, type system
- **Eio Concurrency** - Modern async I/O in OCaml
- **HTTP Protocol** - Understanding HTTP/1.1 specification
- **Parser Combinators** - Building composable parsers

## ⚠️ Important Notes

- **This is a toy project** - Not intended for production use
- **Incomplete implementation** - Many HTTP features are missing
- **Learning-focused** - Code prioritizes clarity over performance
- **No security considerations** - Basic implementation without security features
- **Limited error handling** - Minimal error recovery

## 📚 Resources

- [OCaml Documentation](https://ocaml.org/docs/)
- [Eio Documentation](https://github.com/ocaml-multicore/eio)
- [HTTP/1.1 Specification](https://tools.ietf.org/html/rfc7231)
- [Parser Combinators](https://en.wikipedia.org/wiki/Parser_combinator)

## 📄 License

MIT License - see [LICENSE](LICENSE) file for details.

---

**Built with ❤️ and lots of curiosity about OCaml and web servers.**
