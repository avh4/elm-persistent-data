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
        console.log('404 GET index.html')
      } else {
        var readStream = fs.createReadStream(null, {fd: fd})
        readStream.pipe(response)
        console.log('200 GET index.html')
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
        console.log('404 GET ' + key)
      } else {
        var readStream = fs.createReadStream(null, {fd: fd})
        readStream.pipe(response)
        console.log('200 GET ' + key)
      }
    })
  } else if (request.method === 'PUT') {
    // TODO: validate x-if-empty, x-if-match headers for refs
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
  } else {
    response.writeHead(404)
    response.end('Unexpected method: ' + request.method)
  }
}

var server = http.createServer(handleRequest)

server.listen(PORT, function () {
  console.log('Server listening on: http://localhost:%s', PORT)
})
