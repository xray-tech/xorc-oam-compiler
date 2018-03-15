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

    enum Inter {}
    function inter(bc: ByteCode): Result<Inter, UnknownFFI>

    enum Value {}

    interface InterResult {
        values: [Value]
    }
    function run(inter: Inter): InterResult

    function valueToString(v: Value): string
}
