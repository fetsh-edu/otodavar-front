const path = require('path');

const {merge} = require('webpack-merge');
const common = require('./webpack.common.js');
const webpack = require('webpack');

const dev = {
    mode: 'development',
    plugins: [
        new webpack.ProvidePlugin({
          process: 'process/browser',
        }),
        new webpack.DefinePlugin({
          'process.env': JSON.stringify(process.env),
        })
    ],
    devServer: {
        hot: "only",
        client: {
            logging: "info"
        },
        static: {directory: path.join(__dirname, "../src/assets")},
        devMiddleware: {
            publicPath: "/",
            stats: "errors-only"
        },
        historyApiFallback: true,
    },
};

module.exports = env => {
    const withDebug = !env.nodebug;
    return merge(common(withDebug), dev);
}
