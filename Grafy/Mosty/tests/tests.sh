#!/bin/bash

for i in {1..4}
do
    echo "Test ${i}, expected:"
    cat "tests/graf${i}_expected.txt"
    echo "Got:"
    ./Mosty < "tests/graf${i}.txt"
done