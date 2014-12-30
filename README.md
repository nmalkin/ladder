A Clojure implementation of the
[word ladder](https://en.wikipedia.org/wiki/Word_ladder) game, which uses the
[A\* search algorithm](https://en.wikipedia.org/wiki/A*_search_algorithm)
to find the shortest path.

### Requirements
The program looks for (and uses) the Unix system dictionary,
found at `/usr/share/dict/words`.

### To use
The program has a command-line interface: launch it with `lein run`, then
enter the start and end words at the prompt. Repeat. When you're done, exit
with CTRL-D.
