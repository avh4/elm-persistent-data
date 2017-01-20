var http = require('http')
var fs = require('fs')

const PORT = 8080

function handleRead (key, response) {
  fs.open(key, 'r', function (err, fd) {
    if (err) {
      response.writeHead(404)
      response.end('Read failed: ' + key)
      console.log('404 GET ' + key)
    } else {
      var readStream = fs.createReadStream(null, {fd: fd})
      readStream.pipe(response)
      console.log('200 GET ' + key)
    }
  })
}

function handleWrite (key, request, response) {
  fs.open(key, 'w', function (err, fd) {
    if (err) {
      response.writeHead(500)
      response.end('Write failed: ' + key)
      console.log('500 PUT ' + key)
    } else {
      var writeStream = fs.createWriteStream(null, {fd: fd})
      request.pipe(writeStream)
      request.on('end', function () {
        response.writeHead(204)
        response.end()
        console.log('204 PUT ' + key)
      })
    }
  })
}

function handleRequest (request, response) {
  var path = request.url
  var key

  if (path === '/') {
    handleRead('index.html', response)
  } else if (path.match(/^\/refs\/([-.a-zA-Z0-9]*)$/)) {
    key = path.match(/^\/refs\/([-.a-zA-Z0-9]*)$/)[1]
    if (request.method === 'GET') {
      handleRead('refs/' + key, response)
    } else if (request.method === 'PUT') {
      if (request.headers['x-if-empty']) {
        var exists = fs.existsSync('refs/' + key)
        if (exists) {
          response.writeHead(400)
          response.end('x-if-empty was set, but ref currently exists')
          console.log('400 PUT refs/' + key + ' (already exists)')
        } else {
          handleWrite('refs/' + key, request, response)
        }
      } else if (request.headers['x-if-match']) {
        var actual = fs.readFileSync('refs/' + key, 'utf8')
        var expected = request.headers['x-if-match']
        if (actual !== expected) {
          response.writeHead(400)
          response.end('x-if-match was set, but ref does not match')
          console.log('400 PUT refs/' + key + ' (' + actual + ' !== ' + expected + ')')
        } else {
          handleWrite('refs/' + key, request, response)
        }
      } else {
        response.writeHead(400)
        response.end('x-if-empty or x-if-match header is required')
        console.log('400 PUT refs/' + key)
      }
    } else {
      response.writeHead(400)
      response.end('Unexpected method: ' + request.method)
    }
  } else if (path.match(/^\/content\/(sha256-[a-f0-9]{64})$/)) {
    key = path.match(/^\/content\/(sha256-[a-f0-9]{64})$/)[1]
    if (request.method === 'GET') {
      handleRead('content/' + key, response)
    } else if (request.method === 'PUT') {
      handleWrite('content/' + key, request, response)
    } else {
      response.writeHead(400)
      response.end('Unexpected method: ' + request.method)
    }
  } else {
    response.writeHead(404)
    response.end()
  }
}

var server = http.createServer(handleRequest)

server.listen(PORT, function () {
  console.log('Server listening on: http://localhost:%s', PORT)
})
