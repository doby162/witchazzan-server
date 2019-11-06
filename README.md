[![Build Status](https://travis-ci.com/chrisl8/witchazzan-server.png)](https://travis-ci.com/chrisl8/witchazzan-server)

# Witchazzan - A Game!

This is the start of a game that [doby162](https://github.com/doby162) and [chrisl8](https://github.com/chrisl8) are building.  

The game exists in two parts:  
1. This is the Clojure based server
2. [A Phaser 3 based web front end](https://github.com/chrisl8/witchazzan-client)

Both parts are required for the game to function.

# Server Installation for Running and Development

The Witchazzen Game server is written Clojure, which is required to run the server, along with [Leiningen](https://leiningen.org/).

You can run the `installDependencies.sh` script to attempt to perform these steps automatically. It works on my Digital Ocean droplet, but results can be mixed on personal systems, depending on your environment.

The steps to get this going on your development system are:
1. Install OpenJDK 11 (See below)
2. [Install Clojure](https://clojure.org/guides/getting_started)
3. [Install Leiningen](https://leiningen.org/#install)

## Installing Java (Java Runtime Environment aka. JRE)

The Java Runtime Environment (JRE) is required by Clojure.  

We have had success using OpenJDK 11, which you can install on Ubuntu with:
`sudo apt install openjdk-11-jre`

Make sure it works:
```bash
java --version
```

# Usage

## Create a configuration file
    cp config/.config.clj.default config/config.clj

## Run the server interactively
    lein repl
this starts the server, and also plunks you down into a console where you can manually interact with the code. This will allow the administrator to invoke top level functions, such as setting the framerate of the game or making a player invincible.

Use `Ctrl+d` to quit.

## Contribution

### Format your code like we did
`lein cljfmt fix`

### Lint your code
`lein eastwood`  
But don't be a slave to it, we aren't.

## License

Copyright © 2019 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
