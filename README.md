# virtual-dom

An example of constructing a simple Virtual DOM library

## Overview

This project demonstrates how to build a very simple Virtual DOM library in
pure ClojureScript.

Oritinally presented to [ClojureMN](https://www.meetup.com/clojuremn/) on Nov. 9, 2016.

## Setup

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    lein clean

To create a production build run:

    lein do clean, cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL. 

## License

Copyright © 2016 Peter Schwarz

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
