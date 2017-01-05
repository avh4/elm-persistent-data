var http = require('http')
var fs = require('fs')

const PORT = 8080

function handleRequest (request, response) {
  var key = request.url.slice(1)

  if (key === '') {
    fs.open('index.html', 'r', function (err, fd) {
      if (err) {
        response.writeHead(404)
        response.end('Read failed: index.html')
      } else {
        var readStream = fs.createReadStream(null, {fd: fd})
        readStream.pipe(response)
      }
    })
    return
  }

  if (key.match(/[/.]/)) {
    response.writeHead(400)
    response.end('Path cannot contain \'.\' or \'/\'')
  } else if (request.method === 'GET') {
    fs.open(key, 'r', function (err, fd) {
      if (err) {
        response.writeHead(404)
        response.end('Read failed: ' + key)
      } else {
        var readStream = fs.createReadStream(null, {fd: fd})
        readStream.pipe(response)
      }
    })
  } else if (request.method === 'PUT') {
    fs.open(key, 'w', function (err, fd) {
      if (err) {
        response.writeHead(500)
        response.end('Write failed: ' + key)
      } else {
        var writeStream = fs.createWriteStream(null, {fd: fd})
        request.pipe(writeStream)
        request.on('end', function () {
          response.writeHead(204)
          response.end()
        })
      }
    })
  } else {
    response.writeHead(404)
    response.end('Unexpected method: ' + request.method)
  }
}

var server = http.createServer(handleRequest)

server.listen(PORT, function () {
  console.log('Server listening on: http://localhost:%s', PORT)
})
