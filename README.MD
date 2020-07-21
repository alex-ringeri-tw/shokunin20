
# Shokunin (職人) 20 - Homeschooling

## Running Using Docker
If you have docker installed you can run this without any of the Haskell ecosystem.

First build the docker image.
```
./docker-build.sh
```

Then run it using the provided script:
```
./docker-run.sh <tasks>
```

Pass the tasks as a string argument to the command. for example
```
./docker-run.sh '[5, 4, 1, 2, 7, 8, 3]'
```

You can control the algorithm used to solve the problem with an optional flag:
```
-a (default) : Algorithm that memoizes subsets of tasks that sum to a particular number. (Note: memory usage is proportional to the sum of tasks)"
-b : Algorithm above without memoization"
-c : Depth first search with pruning"
-d : Naive recursive depth first search algorithm"
```

An example run using the depth first search:
```
./docker-run.sh '[5, 4, 1, 2, 7, 8, 3]' -c
```


## Running Using Cabal
```
cabal v2-run shokunin20-exe -- <tasks> [algorithm]
```

