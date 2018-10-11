'use strict';

require('./main.scss');

let elm = require('./Main.elm');

let app = elm.Elm.Main.init({flags: flags});

require('./WebSocketClient.js').subscribe(app);
