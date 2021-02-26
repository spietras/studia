# huffman-coding-x86
Huffman coding in x86 🌳

## Requirements
- ```C``` compiler
- ```x86``` CPU architecture

## Usage

Building the binary:
```
make
```

It will create ```bin``` directory with the binary inside.

Running:
```
./bin/Huffman {encode, decode} INPUT_PATH OUTPUT_PATH
```

In ```encode``` mode ```INPUT_PATH``` is the path to the file to encode. Encoded file will be written to ```OUTPUT_PATH```.

In ```decode``` mode ```INPUT_PATH``` should be a path to an existing encoded file. Decoded file will be written to ```OUTPUT_PATH```.

## Encoding scheme

Encoded file contains data in the following order:

- original file size (8 bytes)
- symbols count (1 byte)
- symbol-frequency table: <symbol (1 byte), frequency (8 bytes)> for each symbol
- encoded data (x bits)
