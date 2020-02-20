[![Build Status](https://travis-ci.com/doby162/witchazzan-server.svg?branch=master)](https://travis-ci.com/doby162/witchazzan-server?branch=master)

# Witchazzan - A Game!

This is the start of a game that [doby162](https://github.com/doby162) and [chrisl8](https://github.com/chrisl8) are building.  

The game exists in two parts:  
1. This is the Clojure based server
2. [A Phaser 3 based web front end](https://github.com/chrisl8/witchazzan-client)

Both parts are required for the game to function.

# Namespace Documents - a contributor's guide 

## Core
This namespace's function is to load all required namespaces and to contain the -main function.
It's role as a top level namespace without any content or dependencies is important for hotloading new code into
a live server instance.

## World
Originally called core, this file contained the entire witchazzan server project, and continues to house anything
not yet big enough to warrant a namespace of it's own. Generally, this file contains the rules and mechanisms of the world.
Many world rules are in the form of helper functions, but the processing of tilemaps, core game loop and server boilerplate also reside here.

The core game loop isn't intuitive, here's a break down:
*Each game piece is a map, with function identifiers and keys.
*Each frame of the simulation, game pieces are fed into their :behavior functions and the return value of that function
is used as the new current state of that object.
*If the object has data in it's :outbox key, that data is attached to it's labeled recipient.
*All objects are then processed with their :handle-inbox functions to alter their state according to received messages. Mail can also be sent to non object entities, such as the object factory, which is how plants and animals reproduce.
*Finally, all objects with :delete-me set to true are garbage collected.

## Comms

This simplest namespace, this file implements the API of the server.
The majority of data passed in by the client application is handled by handle-location-update, which attaches newly received state information to the client's avatar via the mail queue.

The api is, approximately, as follows:
*Handle-login: when a new client is connected, they are assigned an in game avatar and they recieve the output of establish-identity, which allows client programs to know which game object is their avatar.
*Handle-location-update: The main update handler, this function expects new X and Y values for the avatar, as well as
any additional key/value pairs the client program would like to associate with the avatar. It's a current goal of the project to implement validation for this incoming data, by disallowing setting certain keys and validating updates to other keys.
*Handle-chat: Chat messages get broadcast to all clients, they are not stored.
*Handle-command: Various commands can be entered via the client by sending a chat message beginning with a forward slash.
  */look and /listen give information about the location.
  */reload hotloads the current version of the source code into memory, soon to be accompanied by /git-pull
  */who returns a list of all connected players
*Handle-fireball: Creates a new fireball object depending on the state of the originating avatar.

## Common
This namespace contains the global var definitions that need to be accessed in all other files, as well as general functions for accessing this data efficiently. 

## [Behavior](doc/intro.md) 
A collection of high level rules for simulating objects. Some are general, many are specific to a type of plant or animal.
Categorically, there are 
*Default functions, which do not modify the object they are called with.
*Helper functions, which determine facts about the world or an object.
*Behaviors, which return an altered version of the object they are called with.
*Implementations, which are special behaviors built as an interface to an external behavior (such as a behavior that defines being eaten)
*Top level behaviors, such as :behavior and :handle-inbox



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
