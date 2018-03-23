const webpack = require('webpack');

module.exports = {
    mode: 'development',
    entry: {
        "app": "./src/index.ts",
        "editor.worker": 'monaco-editor/esm/vs/editor/editor.worker.js',
        "json.worker": 'monaco-editor/esm/vs/language/json/json.worker',
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
        extensions: [".ts", ".js", ".json"]
    },

    module: {
        rules: [
            { test: /\.ts$/, loader: "ts-loader" },
            { test: /\.css$/, use: ['style-loader', 'css-loader'] },

            // All output '.js' files will have any sourcemaps re-processed by 'source-map-loader'.
            { enforce: "pre", test: /\.js$/, loader: "source-map-loader" },
            {
                test: /\.(scss)$/,
                use: [{
                    loader: 'style-loader', // inject CSS to page
                }, {
                    loader: 'css-loader', // translates CSS into CommonJS modules
                }, {
                    loader: 'postcss-loader', // Run post css actions
                    options: {
                        plugins: function () { // post css plugins, can be exported to postcss.config.js
                            return [
                                require('precss'),
                                require('autoprefixer')
                            ];
                        }
                    }
                }, {
                    loader: 'sass-loader' // compiles Sass to CSS
                }]
            }
        ]
    },
};