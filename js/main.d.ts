
namespace Orcml {
    type Result<T, U> = { ok: T } | { error: U }

    enum Repository { }
    enum ByteCode { }

    interface NoInputError {
        kind: "noinput"
    }

    interface SyntaxError {
        kind: "syntax-error",
        msg: string
    }

    interface UnboundVarError {
        kind: "unbound-var",
        msg: string
    }

    interface UnknownReferedFunction {
        kind: "unknown-refered-funtion",
        msg: string
    }

    interface UnknownFFI {
        kind: "unknown-ffi",
        msg: string
    }

    type CompileError = NoInputError | SyntaxError | UnboundVarError | UnknownReferedFunction

    interface Inter {
        run(): InterResult
    }

    type Value = number | string | boolean | { signal: true } | { closure: true } | { label: true }

    interface InterResult {
        values: [Value]
    }

    interface Orcml {
        makeRepository(): Repository
        compile(repository: Repository, code: string): Result<ByteCode, CompileError>
        inter(bc: ByteCode): Result<Inter, UnknownFFI>

        debugger: Debugger
    }

    enum State { }

    type Pos = { path: string, line: number, col: number }

    type Range = { start: Pos, finish: Pos }

    type Var = number | { index: number, ident: string, pos: Range }

    type EnvValue = { pending: true } | { value: Value }

    interface EnvCell {
        var: Var,
        value: EnvValue
    }

    interface Thread {
        id: number,
        env: EnvCell[],
        pos: Pos
    }

    type Action =
        { publishedValue: Value } |
        { newThread: number } |
        { haltedThread: number } |
        { coeffect: number, thread: number, desc: Value } |
        { ffiError: string, thread: string, args: Value[] }

    interface Debugger {
        init(inter: Inter): { state: State, threads: Thread[] }
        tick(state, thread): { threads: Thread[], trace: Action[] }
    }
}

declare var Orcml: Orcml.Orcml


// module Var : sig
// type t = | Generated of int
//          | Handcrafted of { index : int;
//                             ident : string;
//                             pos : range }
// end

// type op
// type stack
// type state
// type v =
// | Value of Value.t
// | Pending of Value.pending

// type thread = { id : int;
//               op : op;
//               env : (Var.t * v) array;
//               stack : stack;
//               pos : pos}
// type threads = thread list
// type action =
// | PublishedValue of Value.t
// | NewThread of int
// | HaltedThread of int
// | Coeffect of { thread : int; id : int; desc : Value.t }
// | Error of { thread : int; ffi : string; args : Value.t list}


    // function Debugger(inter: Inter) : Debugger

    // interface Debugger {
    //     function 
    // }

