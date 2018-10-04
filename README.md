# Samply Broker UI Material Prototype

This project contains a prototype of a Samply Broker UI which uses [Material Design][1] as basis. Besides that this prototype tries to showcase the following:

* full use of frontend technologies and build tools based on [npm][2] and [webpack][3]
* carried out as [SPA][4] which runs in the browser and connects to backend services like the MDR
* build with [Elm][5] a pure functional and typed language for the browser
* Docker based build for a maximum of reproducibility and independence from different environments
* customizable at deploy time through environment variables

## Usage

To start the UI including the search store which is needed to persist the searches, this project contains a Docker compose file with the complete setup. 

Clone this Git repository, if you haven't already:

```bash
git clone https://github.com/alexanderkiel/samply.broker.ui.material.git
```

Make sure you have port 8080 free or edit the `docker-compose.yml` before brining the system up with:

```bash
docker-compose up
```

Point your browser to: [http://localhost:8080](http://localhost:8080).

## Build

The build is Docker based. Just run:

```bash
docker build .
```

## Development

* install [npm][2]
* run `npm install` to install the required node packages
* run `npm run-script dev` to run an interactive development environment with hot reloading
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
