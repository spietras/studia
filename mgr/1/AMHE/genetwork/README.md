# genetwork

This ```README``` provides info about the development process.

For more info about the package itself see ```genetwork/README.md```.

## Quickstart (on Ubuntu)

```sh
$ apt update && apt install curl git python3 python3-pip python3-venv
$ python3 -m pip install pipx && pipx install poetry
$ pipx ensurepath && exec bash
$ curl -sSL https://repo.anaconda.com/miniconda/Miniconda3-py39_4.9.2-Linux-x86_64.sh -o miniconda.sh
$ bash miniconda.sh && exec bash
(base) $ git clone https://github.com/spietras/genetwork
(base) $ cd genetwork
(base) $ conda env create -f environment.yml
(base) $ conda activate genetwork
(genetwork) $ cd genetwork
(genetwork) $ poetry install --extras dev
(genetwork) $ genetwork
```

## Quickerstart

If you just want to try it out and don't care about polluting your environment:

```sh
$ python3 -m pip install ./genetwork
$ genetwork
```

## Environment management

We are using [```conda```](https://conda.io) for environment management (but you can as well use any other tool, e.g. ```pyenv + venv```). 
The major reason is that ```conda``` lets you specify ```python``` version and will install that version in the environment.
This ensures consistency between different instances (developers, CI, deployment).

The first step is of course to install [```conda```](https://conda.io).

To create an environment, run from project root:

```sh
conda env create -f environment.yml
```

And then activate it by:

```sh
conda activate genetwork
```

Creating the environment is performed only once, but you need to activate it every time you start a new shell.

If the configuration file ```environment.yml``` changes, you can update the environment by:

```sh
conda env update -f environment.yml
```

## Package management

We are using [```poetry```](https://python-poetry.org) to manage our package and its dependencies. 
You need to have it installed outside our environment (I recommend to use [```pipx```](https://pipxproject.github.io/pipx) for that).

To install the package, you need to ```cd``` into ```genetwork``` directory and run:

```sh
poetry install --extras dev --remove-untracked
```

This will download and install all package dependencies (including optional development ones) and install the package in editable mode into the activate environment.

Editable mode means that you don't have to reinstall the package if you change something in the code.
The changes are reflected automatically. 

However, you need to install the package again if you change something in its configuration (e.g. add a new dependency).
But more on that later.

If it's the first time installing the package, ```poetry``` will write specific versions of all packages to ```poetry.lock``` file.
This file should be committed to the repository, so other people can have the exact same versions of all dependencies.
It will work because ```poetry install``` checks if ```poetry.lock``` file is available and uses it if it is.

## Testing

We are using [```pytest```](https://pytest.org) for tests.
It's already installed in the environment, because it's a development-time dependency.
To start first write the tests and put them in ```genetwork/tests```.

To execute the tests, run from project root:

```sh
pytest genetwork
```

## Bulding docs

We are using [```mkdocs```](https://www.mkdocs.org) with [```material```](https://squidfunk.github.io/mkdocs-material) for building the docs.
It lets you write the docs in Markdown format and creates a nice webpage for them.

Docs should be placed in ```genetwork/docs/docs```. 
They are pretty straightforward to write.

To build the docs, ```cd``` into ```genetwork/docs``` and run:

```sh
mkdocs build
```

It will generate ```site``` directory with the webpage source.

## Adding new dependencies

If you need to add a new dependency, look into ```pyproject.toml``` file.
Add it to ```tool.poetry.dependencies``` section.
If it is a development-time dependency you need to mark it as optional and add it to the right groups in ```tool.poetry.extras```.

After that update the installation by running from ```genetwork``` directory:

```sh
poetry update
```

This will install anything new in your environment and update the ```poetry.lock``` file.
Other people only need to run ```poetry install``` to adjust to the incoming changes in the ```poetry.lock``` file.

## Docker

You can build a Docker image of the package (e.g. for deployment).
The build process is defined in ```Dockerfile``` and it's optimized to keep the size small.

To build the image, run from project root:

```sh
 docker build -t genetwork .
```

To also run the container in one go, run:

```sh
docker build -t genetwork . && docker run --rm -it genetwork
```

## Jupyter

You can use [```jupyter```](https://jupyter.org) to experiment with the code and make some great visualizations or reports.

To launch ```jupyter lab``` environment, ```cd``` to ```notebooks``` (although you might do it as well from root if you want) and run:

```sh
jupyter lab
```

The developed package is installed in the environment so we can import it in the notebooks as any other package.
