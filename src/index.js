'use strict';

require('./main.scss');
let WebSocketClient = require('./WebSocketClient.js');

let elm = require('./Main.elm');

let app = elm.Elm.Main.init({flags: flags});

WebSocketClient.subscribe(app);
