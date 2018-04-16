haskell-rl
===

See [repository *index.html* file](https://pages.git.target.com/RedOptHaskell/haskell-rl/) for project description.

~~~
stack build :tic-tac-toe
stack exec tic-tac-toe
pandoc -f markdown+lhs -i app/tictactoe.lhs -t html -o index.html --filter pandoc-include --mathjax
~~~
