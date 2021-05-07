<h1 align="center">textgen</h1>

<div align="center">

[![Running tests](https://github.com/spietras/textgen/actions/workflows/test.yml/badge.svg)](https://github.com/spietras/textgen/actions/workflows/test.yml)
[![Deploying docs](https://github.com/spietras/textgen/actions/workflows/docs.yml/badge.svg)](https://github.com/spietras/textgen/actions/workflows/docs.yml)

</div>

---

Text generation using machine learning.

It lets you easily accomplish the following things:

- [x] **nothing**

But at least it shows some opinionated best practices about python projects.

## Installing

Using ```pip```<sup>*</sup>:

```sh
pip install textgen
```

<sup><sup>* assuming the authors bothered to release the package on PyPI...</sup></sup>

## Usage as a library

**Very** useful example:

```python
from textgen.subpackage.module import identity

x = 1
assert x is identity(x)
```

## Usage as a command line tool

```sh
$ textgen
Hello World.
```