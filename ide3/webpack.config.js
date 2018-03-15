module.exports = {
    mode: 'development',
    entry: "./src/index.ts",
    output: {
        filename: "bundle.js",
        path: __dirname + "/dist"
    },
    
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