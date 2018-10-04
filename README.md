# Samply Broker UI Material Prototype

This project contains a prototype of a Samply Broker UI which uses [Material Design][1] as basis. Besides that this prototype tries to showcase the following:

* full use of frontend technologies and build tools based on [npm][2] and [webpack][3]
* carried out as [SPA][4] which runs in the browser and connects to backend services like the MDR
* build with [Elm][5] a pure functional and typed language for the browser
* Docker based build for a maximum of reproducibility and independence from different environments
* customizable at deploy time through environment variables

## Build

The build is Docker based. Just run:

```bash
docker build .
```

## Development

* install [npm][2]
* run `npm install`
* run `npm run-script dev`
* open [http://localhost:3000](http://localhost:3000) in your browser

## License

Copyright © 2018 Alexander Kiel

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

[1]: <https://material.io>
[2]: <https://www.npmjs.com>
[3]: <https://webpack.js.org>
[4]: <https://en.wikipedia.org/wiki/Single-page_application>
[5]: <https://elm-lang.org>
