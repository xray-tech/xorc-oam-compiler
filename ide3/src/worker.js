import * as worker from 'monaco-editor/esm/vs/editor/editor.worker.js';

importScripts('/js/main.bc.js');

self.onmessage = function () {
    worker.initialize(function (ctx, createData) {
        return Orcml;
    });
};