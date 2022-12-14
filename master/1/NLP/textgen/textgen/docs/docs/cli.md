## Usage as a command line tool

```sh
$ textgen 'I am'
I am alive.
```

Write some initial words and the rest will be automatically generated.

By default it uses the bundled pretrained model. However, you can pass your own model by ```-c``` parameter.

You can also specify the method of picking each word by ```-p``` parameter. 
By default it uses the same method that was used during training.

If you don't pass any words, interactive mode will be used.

## Full usage

```
$ textgen --help
Usage: textgen [OPTIONS] [PROMPT]

  Command line interface for textgen.

Arguments:
  [PROMPT]  Prompt for generation. If None, goes to interactive mode.

Options:
  -c, --cktp FILE             Path to checkpoint path. If None uses bundled
                              one.

  -p, --picker [greedy|prob]  Type of index picking  [default: prob]

  --help                      Show this message and exit.
```
