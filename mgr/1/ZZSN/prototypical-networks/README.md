# 8-Pietras-Sadza

Prototypical network for sound classification 
(_Uczenie klasyfikatorów dźwiękowych niewielką liczbą przykładów z wykorzystaniem sieci prototypowych_).

## Environment management

We are using [```conda```](https://conda.io) for environment management. 
The major reason is that ```conda``` lets you specify ```python``` version and will install that version in the environment.
This ensures consistency between different instances.

The first step is of course to install [```conda```](https://conda.io).

To create an environment, run from project root:

```sh
conda env create -f environment.yml
```

And then activate it by:

```sh
conda activate protosound
```

Creating the environment is performed only once, but you need to activate it every time you start a new shell.

If the configuration file ```environment.yml``` changes, you can update the environment by:

```sh
conda env update -f environment.yml
```

## Package management

All package dependencies are listed in ```requirements.txt``` file.
You should update it if something changes.

The package itself is already installed in the environment.
You can use it as a library or execute the main script.

## Data

You should put your data in some directory.
It should be structured in a specific way.
See ```data/README.md``` for more info and examples.

## Training

You can use the main package script to train the models.

There are three modes: standard classifier (```classic```), standard classifier with limited examples per class (```limited```) and prototypical network (```proto```).

So for example, to train a model you can run:

```sh
protosound classic DATASET_PATH
```

There are some parameters that you can tweak, but the defaults are sensible. 
For more info you can add ```--help``` parameter.

## Logs and results

We are using [```PyTorch Lightning```](https://www.pytorchlightning.ai/) for training.
It automatically saves the best model and training curves.

All of that will be available in the ```lightning_logs``` directory.

You can then use ```tensorboard``` to view the training curves. Just run:

```sh
tensorboard --logdir lightning_logs
```