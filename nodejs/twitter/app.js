var express = require('express');

var app = express();
app.listen(4080);
var tweets = [];

app.get('/', function (req, res) {
    res.send('Welcome to Node Twitter');
});
app.post('/send', express.bodyParser(), function (req, res) {
    if (req.body && req.body.tweet) {
        tweets.push(req.body.tweet);
        res.send({status:'ok', message:'Tweet received'});
    } else {
        res.send({status:'ok', message:'No tweet received'});
    }
});

app.get('/tweets', function (req, res) {
    res.send(tweets);
});
