#!/usr/bin/env bash

CIFAR10_URL="https://www.cs.toronto.edu/~kriz/cifar-10-python.tar.gz"
CIFAR10_FILE="cifar-10-python.tar.gz"
EXTRACT_DIR="cifar-10-batches-py"

# Download CIFAR-10 dataset
if [ ! -f "$CIFAR10_FILE" ]; then
    echo "Downloading CIFAR-10 dataset..."
    wget "$CIFAR10_URL"
else
    echo "CIFAR-10 dataset already downloaded."
fi

# Extract the dataset
if [ ! -d "$EXTRACT_DIR" ]; then
    echo "Extracting CIFAR-10 dataset..."
    tar -xzvf "$CIFAR10_FILE"
else
    echo "CIFAR-10 dataset already extracted."
fi

echo "CIFAR-10 dataset is ready to use."
