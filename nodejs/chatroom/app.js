var net = require('net');

var chatServer = net.createServer();
var clientList = [];

chatServer.on('connection', function (client) {
    client.name = client.remoteAddress + ':' + client.remotePort;
    client.write('Hi ' + client.name + '!\n');
    clientList.push(client);
    client.on('data', function (data) {
        cleanup = [];
        for (var i = 0; i < clientList.length; i++) {
            if (clientList[i] != client) {
                if (clientList[i].writable) {
                    clientList[i].write(client.name + ' says ' + data);
                } else {
                    cleanup.push(clientList[i]);
                    clientList[i].destroy();
                }
            }
        }
    });
    client.on('end', function () {
        console.log(client.name + ' quit');
        clientList.splice(clientList.indexOf(client), 1);
    });
    client.error('error', function () {
        console.log(e);
    });
});

chatServer.listen(4080);
