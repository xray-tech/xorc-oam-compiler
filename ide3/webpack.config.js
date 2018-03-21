const webpack = require('webpack');

module.exports = {
    mode: 'development',
    entry: {
        "app": "./src/index.ts",
        "editor.worker": 'monaco-editor/esm/vs/editor/editor.worker.js',
    },
    output: {
        filename: "[name].bundle.js",
        path: __dirname + "/dist"
    },

    plugins: [
        // Ignore require() calls in vs/language/typescript/lib/typescriptServices.js
        new webpack.IgnorePlugin(
            /^((fs)|(path)|(os)|(crypto)|(source-map-support))$/,
            /vs\/language\/typescript\/lib/
        )
    ],
    
    devServer: {
        contentBase: [
            __dirname,
            __dirname + "/../_build/default"
        ]
    },
    
    // Enable sourcemaps for debugging webpack's output.
    devtool: "source-map",
    
    resolve: {
        // Add '.ts' ayarnd '.tsx' as resolvable extensions.
        extensions: [".ts",  ".js", ".json"]
    },
    
    module: {
        rules: [
            { test: /\.ts$/, loader: "ts-loader" },
            { test: /\.css$/, use: [ 'style-loader', 'css-loader' ] },
            
            // All output '.js' files will have any sourcemaps re-processed by 'source-map-loader'.
            { enforce: "pre", test: /\.js$/, loader: "source-map-loader" }
        ]
    },
};