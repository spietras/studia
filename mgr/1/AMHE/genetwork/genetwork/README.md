<h1 align="center">genetwork</h1>

<div align="center">

[![Running tests](https://github.com/spietras/genetwork/actions/workflows/test.yml/badge.svg)](https://github.com/spietras/genetwork/actions/workflows/test.yml)
[![Deploying docs](https://github.com/spietras/genetwork/actions/workflows/docs.yml/badge.svg)](https://github.com/spietras/genetwork/actions/workflows/docs.yml)

</div>

---

Satisfying network demands using genetic algorithm.

It lets you easily accomplish the following things:

- [x] **nothing**

But at least it shows some opinionated best practices about python projects.

## Installing

Using ```pip```<sup>*</sup>:

```sh
pip install genetwork
```

<sup><sup>* assuming the authors bothered to release the package on PyPI...</sup></sup>

## Usage as a library

**Very** useful example:

```python
from genetwork.subpackage.module import identity

x = 1
assert x is identity(x)
```

## Usage as a command line tool

```sh
$ genetwork
Hello World.
```