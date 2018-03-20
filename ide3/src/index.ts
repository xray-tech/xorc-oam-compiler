/// <reference types="../../_build/default/js/main" />

import CM = require("codemirror");
import * as Rx from 'rxjs/Rx'

import 'codemirror/lib/codemirror.css';

function isOk<T,U>(v: Orcml.Result<T,U>): v is { ok: T }  {
    return (<{ ok: T } >v).ok !== undefined;
}

function unwrap<T,U>(v: Orcml.Result<T,U>): T {
    if (isOk(v)) {
        return v.ok
    } else {
        throw Error("Unwrap error")
    }
}

let repo = Orcml.makeRepository()
let bc = Orcml.compile(repo, "1 | 2.2 | \"hello world\"")
let inter = Orcml.inter(unwrap(bc))
let res = unwrap(inter).run()

console.log("--- values", res.values)
class State {

}

class Mode {
    token(stream: CM.StringStream, state: State): string | null {
        stream.next()
        return "comment"
    }
}

let OrcModeFactory: CM.ModeFactory<State>
OrcModeFactory = function(config, options) {
    return new Mode()
}

CM.defineMode("orc", OrcModeFactory)

let editor = CM(document.getElementById("editor"), { mode: "orc" })