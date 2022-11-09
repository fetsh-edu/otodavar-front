const path = require('path');

const {merge} = require('webpack-merge');
const common = require('./webpack.common.js');


const dev = {
    mode: 'development',
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
