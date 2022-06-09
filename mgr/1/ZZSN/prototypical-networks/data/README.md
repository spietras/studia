# Data

This ```README``` provides some about structuring data.

## ```audio```

You should put your sound files in the ```audio``` directory.
They should be ```.wav``` files, but you can name them whatever you like.

## ```whitelist.txt```

Only class labels listed here are considered during training.
That way you can easily filter classes from the dataset.

Put one label per one line without any extra characters.

## ```labels.csv```

Put labels of each audio file here.
It's a ```.csv``` file with ```fname``` and ```labels``` headers.

Under ```fname``` you should put your audio files' names without the extension.
So ```audio/1.wav``` should be listed as ```1```.

In ```labels``` you should put each audio labels. 
There can be multiple labels, but only one label in considered valid.
More specifically a sample is considered valid if it has only one of its labels present in the ```whitelist.txt``` file.
Of course, you can put only one label here, but data from ```FSD50K``` is hierarchically labeled so for convenience we are also parsing multiple labels.
Multiple labels should be separated by comma, but that's also ```.csv``` separator, so you should put the labels in quotes.