# Build management
We'll be using [cabal](https://www.haskell.org/cabal/) with `hpack` to manage project builds. Run:

```
cabal install hpack 
```
To install `hpack`. We can now use a simpler `package.yaml` file to list build dependencies, executable information, etc, and the .cabal file will get generated _automatically_ for us. (Note: this means that you DO NOT edit the .cabal file, as it will likely not even work. Always change the .yaml file.)

To build the project, run
```
hpack
cabal build
```
Before you start the load testing script, we need to setup a dummy server

# Start Dummy FastAPI Server
Make sure to have the python packages `fastapi` and `uvicorn` installed in your current environment. Run

```
make single
```

This starts up a single server at the default port 8000.

# Run load testing

To execute the `Main.hs` script, run:
```
cabal run roebling-exe  -- --method GET http://localhost:8000/slow
```

# Run plotting demo

To see the current plotting demo, run:
```
cabal run roebling-exe  -- --method GET http://localhost:8000/slow --plotDemo
```


# Makefile shorthand

The full set of commands is available as a shorthand via the Makefile. Once you make edits, you can simply run:
```
make single # start fastapi server, if not already started
make dev
```