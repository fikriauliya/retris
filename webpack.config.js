const path = require('path');

module.exports = {
  entry: {
    retris: './lib/js/src/retris/RootComp.js',
  },
  output: {
    path: path.join(__dirname, "bundledOutputs"),
    filename: '[name].js',
  },
};
