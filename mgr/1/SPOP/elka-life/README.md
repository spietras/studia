# elka-life

This ```README``` provides info about the development process.

For more info about the package itself see ```textgen/README.md```.

## Quickstart (on Ubuntu)

Assuming you already have the source code:

```sh
$ apt update && apt install curl
$ curl -sSL https://get.haskellstack.org | sh
$ stack build
$ stack exec elka-life
```

## Package management

We are using [```stack```](https://www.haskellstack.org) to manage our package and its dependencies. You need to have it installed.

To build the package run from project root:

```sh
stack build
```

This will download the right Haskell compiler needed for our package (and keep it isolated from system) and package dependencies (if any).

Then you can run the package executable by:

```sh
stack exec elka-life
```

If you change something in package configuration or code, you need to build the package again.

## Docker

If you don't want to install ```stack``` or you want to deploy the app you can use ```Docker```.
The build process is defined in ```Dockerfile``` and it's optimized to keep the size small.

To build the image, run from project root:

```sh
 docker build -t elka-life .
```

To also run the container in one go, run:

```sh
docker build -t elka-life . && docker run --rm -it elka-life
```
