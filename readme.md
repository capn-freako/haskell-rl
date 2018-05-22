haskell-rl
===

See [repository *index.html* file](https://pages.git.target.com/RedOptHaskell/haskell-rl/) for project description.

Instant Gratification Instructions:

~~~
stack build
stack exec <app>  (<app>: "example", "tictactoe", "rentalcars", "gambler", "blackjack")
pandoc -f markdown+lhs -i app/<app>.lhs -t html -o <app>.html --filter pandoc-include --mathjax
(View file "<app>.html" in your browser.)
~~~

