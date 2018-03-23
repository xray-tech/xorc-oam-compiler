/// <reference types="../../_build/default/js/main" />

import 'bootstrap';
import "./css/main.scss"
import * as monaco from 'monaco-editor';

// Since packaging is done by you, you need
// to instruct the editor how you named the
// bundles that contain the web workers.

(<any>self).MonacoEnvironment = {
    getWorkerUrl: function (moduleId: string, label: string) {
        return './editor.worker.bundle.js';
    }
}

import * as Rx from 'rxjs/Rx'


function isOk<T, U>(v: Orcml.Result<T, U>): v is { ok: T } {
    return (<{ ok: T }>v).ok !== undefined;
}

function unwrap<T, U>(v: Orcml.Result<T, U>): T {
    if (isOk(v)) {
        return v.ok
    } else {
        throw Error("Unwrap error")
    }
}

let repo = Orcml.makeRepository()
// let bc = Orcml.compile(repo, "1 | 2.2 | \"hello world\"")
// let inter = Orcml.inter(unwrap(bc))
// let res = unwrap(inter).run()

// console.log("--- values", res.values)

let tokenizer = {
    keywords: [
        "true", "false", "null", "signal", "stop", "type", "val", "lambda", "as", "def", "sig",
        "if", "then", "else"
    ],
    symbols: /[=><!~?:&|+\-*\/\^%\.]+/,
    escapes: /\\(?:[btnfr\\"])/,

    keywordops: [
        '::', ':!:'
    ],
    defaultToken: "invalid",
    autoClosingPairs: [
        { open: '{', close: '}' },
        { open: '[', close: ']' },
        { open: '(', close: ')' },
        { open: '"', close: '"' },
        { open: '\'', close: '\'' },
    ],
    brackets: [
        {
            open: '(',
            close: ')',
            token: "delimiter.parenthesis"
        },
        {
            open: '[',
            close: ']',
            token: "delimiter.parenthesis"
        },
        {
            open: '{.',
            close: '.}',
            token: "delimiter.record"
        }
    ],
    tokenizer: {
        root: [
            {
                regex: /[a-zA-Z_][_a-zA-Z0-9']*/,
                action: {
                    cases: {
                        "@keywords": "keyword",
                        "@default": "identifier",
                    }
                }
            },
            {
                regex: "refer from",
                action: { token: "keyword" }
            },
            {
                regex: /{\.|\.}|[()\[\]]/,
                action: { token: "@brackets" }
            },
            { include: '@whitespace' },
            {
                regex: /@symbols/,
                action: {
                    cases: {
                        '@keywordops': 'keyword',
                        '@default': 'operator'
                    }
                }
            },
            {
                regex: /\d*\.\d*([Ee][-+]?\d+)?/,
                action: { token: "number.float" }
            },
            {
                regex: /0|[1-9]\d*/,
                action: { token: "number.integer" }
            },
            {
                regex: /[;,]/,
                action: { token: "delimiter" }
            },
            {
                regex: /"/,
                action: {
                    token: "string.delim",
                    bracket: "@open",
                    next: "@string"
                }
            }],
        string: [
            { regex: /[^\\"']+/, action: { token: "string" } },
            { regex: /@escapes/, action: { token: "string.escape" } },
            { regex: /\\./, action: { token: "string.escape.invalid" } },
            {
                regex: /"/,
                action: {
                    token: "string.delim",
                    next: "@pop"
                }
            }
        ],

        whitespace: [
            { regex: /[ \t\r\n]+/, action: { token: "white" } },
            { regex: /--.*$/, action: { token: "comment" } },
            { regex: /{-/, action: { token: "comment", next: "@comment" } },
        ],

        comment: [
            { regex: /[^{-]/, action: { token: "comment" } },
            { regex: /{-/, action: { token: "comment", next: "@push" } },
            { regex: /-}/, action: { token: "comment", next: "@pop" } },
            { regex: /[{-]/, action: { token: "comment" } },
        ],
    },

    tokenPostfix: ".orc",
}

// class Mode {
//     token(stream: CM.StringStream, state: State): string | null {
//         stream.next()
//         return "comment"
//     }
// }

// let OrcModeFactory: CM.ModeFactory<State>
// OrcModeFactory = function(config, options) {
//     return new Mode()
// }

// CM.defineMode("orc", OrcModeFactory)

// CM.defineSimpleMode("hello")

monaco.languages.register({ id: "orc" })

monaco.languages.json

monaco.languages.setMonarchTokensProvider("orc", tokenizer)

let richEditConfiguration: monaco.languages.LanguageConfiguration = {
    wordPattern: /(-?\d*\.\d\w*)|([^\[\{\]\}\:\"\,\s]+)/g,
    comments: {
        lineComment: '//',
        blockComment: ["{-", "-}"]
    },
    brackets: [
        ['{.', '.}'],
        ['(', ')'],
        ['[', ']']
    ],
    autoClosingPairs: [
        { open: '{.', close: '.}', notIn: ['string'] },
        { open: '(', close: ')', notIn: ['string'] },
        { open: '[', close: ']', notIn: ['string'] },
        { open: '"', close: '"', notIn: ['string'] }
    ]
};

monaco.languages.setLanguageConfiguration("orc", richEditConfiguration)

let editor = monaco.editor.create(document.getElementById('editor'), {
    value: `console.log("Hello, world")
    asd asd asd asd asd asda sd a
    asd asd asd asd asd asda sd a
    
    asdfasdf
    asdf`,
    minimap: { enabled: false },
    language: "orc",
    autoClosingBrackets: true,
    matchBrackets: true,
    // scrollbar: { vertical: "hidden" },
    overviewRulerBorder: false
});

let runs = Rx.Observable.fromEvent(document.getElementById("run"), "click")

let model = editor.getModel()

runs.subscribe(() => {
    let code = editor.getValue()
    let bc = Orcml.compile(repo, code)
    let inter = Orcml.inter(unwrap(bc))
    let res = unwrap(inter).run()
})


let changes = new Rx.Observable(function (observer) {
    let disp = model.onDidChangeContent(e => observer.next(e));
    () => disp.dispose()
})

changes.subscribe(x => console.log(x))

let worker = monaco.editor.createWebWorker({ moduleId: "worker", label: "orc" })

monaco.editor.setModelMarkers(model, "linter",
    [{
        severity: monaco.Severity.Error,
        message: "Ooooh",
        startLineNumber: 2,
        endLineNumber: 2,
        startColumn: 5,
        endColumn: 10
    }])