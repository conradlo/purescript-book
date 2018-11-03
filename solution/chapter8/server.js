var express = require('express');
var app = express();

app.use(express.static('./html'))
app.listen(1337);