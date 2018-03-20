declare namespace Orcml {

    type Result<T, U> = { ok: T } | { error: U }

    enum Repository { }
    enum ByteCode { }
    function makeRepository(): Repository

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
    function compile(repository: Repository, code: string): Result<ByteCode, CompileError>

    function inter(bc: ByteCode): Result<Inter, UnknownFFI>

    interface Inter {
        run(): InterResult
    }

    type Value = number | string | boolean | { signal: true } | { closure: true } | { label: true }

    interface InterResult {
        values: [Value]
    }
    

    // function Debugger(inter: Inter) : Debugger

    // interface Debugger {
    //     function 
    // }
}
