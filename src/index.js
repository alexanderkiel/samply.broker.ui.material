'use strict';

require('./main.scss');

let elm = require('./Main.elm');

let app = elm.Elm.Main.init({flags: flags});

/*app.ports.sendMsg.subscribe(function (msg) {
    switch (msg.tag) {
        case 'StartNewSearch':
            console.log('StartNewSearch');
            try {
                //localStorage.setItem('upload', JSON.stringify(msg.data));
                app.ports.receiveMsg.send({
                    tag: 'NewSearchId',
                    data: "1"
                });
            } catch (error) {
                console.error(error);
            }
            break;
        default:
            console.error('Received unknown message', msg)
    }
});*/
