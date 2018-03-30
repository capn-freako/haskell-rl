haskell-rl
===

[![Build Status](https://travis-ci.org/capn-freako/haskell-rl.png)](https://travis-ci.org/capn-freako/haskell-rl)

See https://capn-freako.github.io/haskell-rl/index.html for project description.

~~~
stack build --test --exec "$(stack path --local-install-root)/bin/haskell-rl-example" --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i app/example.lhs -t html -o index.html --filter pandoc-include --mathjax" --file-watch
~~~
