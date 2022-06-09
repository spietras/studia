# Usage as a command line tool

```sh
$ genetwork network.xml -o results.json
Generation number: 1, best score: 82030.0, best valid: False
...
```

Pass in SNDLib network definition file.

You can write results to some file by passing ```-o``` parameter.
Without this results are printed to stdout.

## Full usage

```
$ genetwork --help
Usage: genetwork [OPTIONS] NETWORK_PATH

  Command line interface for genetwork.

Arguments:
  NETWORK_PATH  Path to graph definition file  [required]

Options:
  -i, --max-iters INTEGER         Maximum iterations, if None stops when no
                                  changes occur

  -p, --pop-size INTEGER          Initial population size  [default: 500]
  -t, --selection-threshold FLOAT
                                  Percent of population that will participate
                                  in selection  [default: 0.75]

  -f, --selection-factor FLOAT    Relative size of selected population
                                  [default: 2]

  -m, --pm FLOAT                  Probability of mutation  [default: 0.002]
  -c, --pc FLOAT                  Probability of crossover  [default: 0.99]
  -a, --alpha FLOAT               Alpha parameter of the objective function
                                  (undersupplying)  [default: 25]

  -b, --beta FLOAT                Beta parameter of the objective function
                                  (exceeding fiber capacity)  [default: 7.5]

  -l, --lam INTEGER               Maximum capacity of wavelengths (lambdas) in
                                  a single fiber  [default: 96]

  -P, --max-paths INTEGER         Maximum paths to generate if there are no
                                  admissiblePaths in XML  [default: 4]

  -T, --max-trans INTEGER         Maximum number of transponder on a path that
                                  can be drawn during initialization
                                  [default: 1]

  -s, --succ-prob FLOAT           Success probability used in geometric
                                  distribution while drawing the number of
                                  transponders on a path  [default: 0.9]

  -c, --config-path FILE          Path to configuration file
  -o, --out-path PATH             Output file path, if None prints to stdout
  --install-completion            Install completion for the current shell.
  --show-completion               Show completion for the current shell, to
                                  copy it or customize the installation.

  --help                          Show this message and exit.
```