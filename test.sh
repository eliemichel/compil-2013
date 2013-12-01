#!/bin/bash

for f in tests/exec/*.cpp
do
	echo "Processing $f"
	./minic++ $f
done
