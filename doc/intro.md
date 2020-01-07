# building AI game pieces in Witchazzan

## definition of a game piece

At the time of writing, an AI entity, be it animal, plant or flame,
is defined by a map containing properties, methods and genes.

Game pieces, have no formal schema. The closest thing
they have to one is a spawn-function, such as spawn-slime.

```(defn spawn-slime
  "create a slime in the world"
  [scene & mods]
  (add-game-piece!
   (conj
    (find-empty-tile scene)
    {:scene scene
     :sprite "gloobScaryman"
     :type "slime"
     :energy 24
     :behavior "witchazzan.core/slime-behavior"
     :hourly "witchazzan.core/slime-hourly"
     :reproduce "witchazzan.core/plant-reproduce"
     :hunt "witchazzan.core/slime-hunt"
     :clock 1
     :handle-mail "witchazzan.core/slime-inbox"
     :max-speed 4
     :genes
     (generate-genes
      :speed :repro-threshold :repro-chance)}
    (first mods)))) ; passed params overwrite anything
```

These spawn functions are shortcuts for the admin function add-game-piece!, so they return the ID of the newly created object.

Note that the genes are defined as a list of keys, each gene is
generated from scratch for new game pieces.

`:behavior` and `:handle-mail` are required by the core game loop as infrastructure.

All keys not listed are nil by default. Slimes may be capable of having
an `:outbox` property, which allows them to interact with other game pieces through the mail queue, but we needn't initialize it here.

Though not required, having a `:clock` key and an `:hourly` method is
idiomatic. Most objects need to do some tasks less often than others, such as make choices or calculate energy consumption

Children of a game piece have only their parent(s) as a schema.
This can lead to old data being persisted through outdated game
pieces hanging around. Reproduction is handled through the outbox, making use of the mutate-genes helper

## core game loop

Each game piece has a behavior method. This is a function that takes one moment of state, and returns the
next moment of state. All game-piece state is maps, and the behavior function coordinates all
changes that the object makes to itself and to the outside world.

Typically a behavior function would look like this fictional example

```
(defn grass-behavior
    [this]
    (as->
    this t
    (photosynth t)
    (grow t)
    (reproduce t)
    (send-nutrients-to-mushrooms t)))
```

`as->` is a general case of the <a href="https://clojure.org/guides/threading_macros"> threading macro</a> 
it takes a piece of data to operate on, and then specifies a short hand for it. The short hand is passed
from function to function, processing one element of behavior at a time

Typically, a function that represents a single aspect of behavior looks like this:

```
(defn check-starve [t]
  (cond
    (>= 0 (:energy t))
    (merge t {:delete-me true})
    :else t))
```

The `merge` function overwrites and adds values to the first map specified with the subsequent maps,
and it's the way we incrementally modify our maps.

Sometimes an ai function doesn't need to return a modified version of it's input because it's purely informational.
they have no particular format, but tend to take an object as the argument all the same.


```
(defn gene-speed
  "determines the speed a creature should have based
  on speed stats and a hard max"
  [this]
  (quot
   (:speed (:genes this))
   (quot (setting "gene-max") (:max-speed this))))

```

All objects must also have an inbox function. When other game-pieces interact with our game-piece,
they do so via the mail queue, which adds maps to out `:inbox`. Like `:behavior`, it allows objects to calculate their next state, but it does so on the basis of messages rather than the state of the world.

In this inbox handler, we die if any attack is received, and clear our inbox otherwise.
`:delete-me` is a special key that the `collect-garbage!` function looks for. 
```
(defn carrot-inbox
  [this]
  (let [hits (filter #(= (:method %) "hit") (:inbox this))]
    (cond
      (> (count hits) 0)
      (merge this {:delete-me true})
      :else
      (merge this {:inbox nil}))))
```


## organization

Typically, all code written for game-pieces to modify their own or other's state should be in the behavior file.

However, since there is not yet any strict delineation, they may also access the core functions directly.

One must never call any function that mutates state, such functions are marked with a bang (!)
doing so invalidates the game state and causes the simulation to stall out.

wWhen new core functionality is used, a wrapper is best practice.
this function calls out to a method on the tilemap
```
(defn teleport [this]
  "check for and apply teleports"
  (when
   (not (:teleport-debounce this))
    ((:teleport (name->scene (:scene this))) this)))
```

## tools

Inside of a thread macro, every function's return value is used, making printing to the prompt
needlessly cumbersome. Use `(thread-debug data)` to print from a thread without effecting the logic chain at all.

`(pp/pprint data)` is the easiest to read format for logging data
`(println data)` is easier to type
`(log data)` writes to a file instead of printing

`(setting "pause" true)` ends the game loop.
You are then free to modify state, such as by spawning in new pieces or modifying them with
`(update-game-piece!)`, or by inspecting them with functions like `(id->piece)` or
`(pp/pprint (filter #(= "carrot" (:type %)) (objects)))` to see all pieces of a specific type

move through time one tick at a time with `(game-loop)`

restart the automatic game loop with `(setting "pause" false)` `(threadify game-loop)`
