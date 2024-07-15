#!/usr/bin/env bash

# MNIST dataset URLs
MNIST_BASE_URL="http://yann.lecun.com/exdb/mnist"
TRAIN_IMAGES="train-images-idx3-ubyte.gz"
TRAIN_LABELS="train-labels-idx1-ubyte.gz"
TEST_IMAGES="t10k-images-idx3-ubyte.gz"
TEST_LABELS="t10k-labels-idx1-ubyte.gz"

# Create a directory for the dataset
mkdir -p mnist_data
cd mnist_data

# Download function
download_and_extract() {
    local filename=$1
    if [ ! -f "${filename%.gz}" ]; then
        echo "Downloading $filename..."
        wget "$MNIST_BASE_URL/$filename"
        echo "Extracting $filename..."
        gunzip -k "$filename"
    else
        echo "$filename already downloaded and extracted."
    fi
}

# Download and extract each file
download_and_extract $TRAIN_IMAGES
download_and_extract $TRAIN_LABELS
download_and_extract $TEST_IMAGES
download_and_extract $TEST_LABELS

echo "MNIST dataset download and extraction complete."
