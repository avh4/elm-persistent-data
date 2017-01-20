var fs = require('fs')
var path = require('path')
var morgan = require('morgan')
var express = require('express')
var app = express()

app.use(morgan('tiny'))

const PORT = process.env.PORT || 8080

app.use(function (req, res, next) {
  res.header('Access-Control-Allow-Origin', '*')
  res.header('Access-Control-Allow-Methods', 'GET, PUT')
  res.header('Access-Control-Allow-Headers', 'Origin, X-Requested-With, Content-Type, Accept, x-if-match, x-if-empty')
  next()
})

function handleWrite (key, req, res) {
  fs.open(key, 'w', function (err, fd) {
    if (err) {
      throw err
    } else {
      var writeStream = fs.createWriteStream(null, {fd: fd})
      req.pipe(writeStream)
      req.on('end', function () {
        res.sendStatus(204)
      })
    }
  })
}

app.get('/', function (req, res) {
  res.sendFile('index.html', { root: __dirname })
})

app.get('/refs/:key', function (req, res) {
  if (req.url.match(/^\/refs\/([-.a-zA-Z0-9]*)$/)) {
    res.sendFile(req.params.key, { root: path.join(__dirname, '/refs') })
  } else {
    res.sendStatus(404)
  }
})

app.put('/refs/:key', function (req, res) {
  if (req.url.match(/^\/refs\/([-.a-zA-Z0-9]*)$/)) {
    var key = req.params.key
    if (req.headers['x-if-empty']) {
      var exists = fs.existsSync('refs/' + key)
      if (exists) {
        res.status(400).send('x-if-empty was set, but ref currently exists')
        // console.log('400 PUT refs/' + key + ' (already exists)')
      } else {
        handleWrite('refs/' + key, req, res)
      }
    } else if (req.headers['x-if-match']) {
      var actual = fs.readFileSync('refs/' + key, 'utf8')
      var expected = req.headers['x-if-match']
      if (actual !== expected) {
        res.status(400).send('x-if-match was set, but ref does not match')
        // console.log('400 PUT refs/' + key + ' (' + actual + ' !== ' + expected + ')')
      } else {
        handleWrite('refs/' + key, req, res)
      }
    } else {
      res.status(400).send('x-if-empty or x-if-match header is required')
    }
  } else {
    res.sendStatus(404)
  }
})

app.get('/content/:key', function (req, res) {
  if (req.url.match(/^\/content\/(sha256-[a-f0-9]{64})$/)) {
    res.sendFile(req.params.key, { root: path.join(__dirname, '/content') })
  } else {
    res.sendStatus(404)
  }
})

app.put('/content/:key', function (req, res) {
  if (req.url.match(/^\/content\/(sha256-[a-f0-9]{64})$/)) {
    handleWrite('content/' + req.params.key, req, res)
  } else {
    res.sendStatus(404)
  }
})

app.listen(PORT, function () {
  console.log('Server listening on: http://localhost:%s', PORT)
})
