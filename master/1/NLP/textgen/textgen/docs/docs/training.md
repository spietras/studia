## Training

To train a model, use the ```textgen-train``` command.

```sh
$ textgen-train DATASET_PATH
```

You need to pass a path to the directory containing your data.
There should be ```train```, ```val```, ```test``` subdirectories inside the passed directory.
Each subdirectory should contain files with text for that particular dataset.

After training, the best model will be stored in ```lightning_logs``` directory.

## Full usage

```
$ textgen-train --help
Usage: textgen-train [OPTIONS] DATASET_PATH

  Command line interface for textgen-train.

Arguments:
  DATASET_PATH  Path to dataset with train/val/test subdirectories for
                training.  [required]


Options:
  -c, --ckpt-path FILE            Path to checkpoint file on which training
                                  will continue.

  -s, --max-length INTEGER        Maximum sentence length in words.  [default:
                                  20]

  -b, --batch-size INTEGER        Batch size.  [default: 64]
  -w, --num-workers INTEGER       Number of Dataloader workers  [default: 4]
  -g, --num-gpus INTEGER          Number of GPUs.  [default: 1]
  -e, --max-epochs INTEGER        Maximum number of training epochs.
                                  [default: 30]

  --lr FLOAT                      Learning rate.  [default: 0.01]
  -m, --d-model INTEGER           The size of the vector representing one
                                  token.  [default: 64]

  -f, --d-ff INTEGER              The size of the first layer in the FFN in
                                  each layer of the encoder/decoder stack.
                                  [default: 16]

  -h, --num-heads INTEGER         The number of attention heads.  [default: 4]
  -l, --num-layers INTEGER        The number of encoder/decoder stack layers.
                                  [default: 4]

  -d, --dropout-rate FLOAT        Dropout rate.  [default: 0.1]
  -p, --index-picker [greedy|prob]
                                  Type of index picking  [default: greedy]
  -t, --teacher-forcing-val       Use teacher forcing in validation and test
                                  stages  [default: False]

  -r, --seed INTEGER              Seed for reproducibility

  --help                          Show this message and exit.
```
