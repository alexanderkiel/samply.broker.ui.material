const path = require("path");
const webpack = require("webpack");
const HTMLWebpackPlugin = require("html-webpack-plugin");

const html = new HTMLWebpackPlugin({
    template: 'src/index.html',
    inject: 'body'
});

const sassRule = {
    test: /\.(scss)$/,
    exclude: [/elm-stuff/, /node_modules/],
    use: [
        {
            loader: 'style-loader' // inject CSS to page
        }, {
            loader: 'css-loader' // translates CSS into CommonJS modules
        }, {
            loader: 'sass-loader',
            options: {
                includePaths: ['./node_modules']
            }
        }
    ]
};

const devOutput = {
    path: path.join(__dirname, "dist"),
    publicPath: '/',
    filename: '[name].js',
};

const prodOutput = {
    path: path.join(__dirname, "dist"),
    publicPath: '/',
    filename: '[name]-[hash].js',
};

const elmDevRule = {
    test: /\.elm$/,
    exclude: [/elm-stuff/, /node_modules/],
    use: [{
        loader: 'elm-hot-webpack-loader'
    }, {
        loader: 'elm-webpack-loader',
        options: {
            debug: false
        }
    }]
};

const elmProdRule = {
    test: /\.elm$/,
    exclude: [/elm-stuff/, /node_modules/],
    use: [{
        loader: 'elm-webpack-loader',
        options: {
            debug: false,
            optimize: true
        }
    }]
};

const devServer = {
    inline: true,
    hot: true,
    stats: {colors: true},
    historyApiFallback: true,
    proxy: {
        "/api": {
            target: "http://localhost:8080",
            pathRewrite: {"^/api": ""}
        }
    }
};

module.exports = (env, argv) => {
    if (argv.mode === 'development') {
        return {
            plugins: [
                new webpack.HotModuleReplacementPlugin(),
                html
            ],

            output: devOutput,

            module: {
                rules: [
                    sassRule,
                    elmDevRule
                ]
            },

            devServer: devServer
        }
    }

    if (argv.mode === 'production') {
        return {
            plugins: [
                html
            ],

            output: prodOutput,

            module: {
                rules: [
                    sassRule,
                    elmProdRule
                ]
            }
        }
    }
};
