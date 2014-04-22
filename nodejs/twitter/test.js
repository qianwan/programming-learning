var http = require('http');
var assert = require('assert');

var opts = {
    host: 'localhost',
    port: 4080,
    path: '/send',
    method: 'POST',
    headers: {'content-type': 'application/x-www-form-urlencoded'}
};

var req = http.request(opts, function (res) {
    res.setEncoding('utf8');
    var data = '';
    res.on('data', function (d) {
        data = data + d;
    });
    res.on('end', function () {
        assert.strictEqual(data, '{\n  "status": "ok",\n  "message": "Tweet received"\n}');
    });
});

req.write('tweet=what the fuct');
req.end();
