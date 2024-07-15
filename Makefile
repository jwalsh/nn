CC      = gcc
LD      = gcc
# TODO: Remove Homebrew specific include path
EMACS_INCLUDE = /opt/homebrew/Cellar/emacs/29.4/include
CFLAGS  = -ggdb3 -Wall -fPIC -I./include -I$(EMACS_INCLUDE)
LDFLAGS =

MERMAID_FILES := $(wildcard diagrams/*.mmd)
PNG_FILES := $(patsubst diagrams/%.mmd,images/%.png,$(MERMAID_FILES))

# TODO: Use https://conx.readthedocs.io/en/latest/MNIST.html to download MNIST data
MNIST_BASE_URL = http://yann.lecun.com/exdb/mnist
MNIST_FILES = train-images-idx3-ubyte.gz train-labels-idx1-ubyte.gz t10k-images-idx3-ubyte.gz t10k-labels-idx1-ubyte.gz
MNIST_EXTRACTED = $(patsubst %.gz,%,$(MNIST_FILES))


all: matrix.so ops.so

clean:
	rm -f *.so *.o
	rm -f images/*.png
	rm -f $(MNIST_FILES) $(MNIST_EXTRACTED)


# Make shared library out of the object file
%.so: %.o
	$(LD) -shared $(LDFLAGS) -o $@ $< utils.o
	rm utils.o

# Compile source file to object file
%.o: src/%.c
	$(CC) $(CFLAGS) -fPIC -c src/utils.c $< 

# Test
test:
	emacs -batch \
		--eval "(add-to-list 'load-path \".\")" \
		-l ert \
		-l nn.el \
		-l tests.el \
		-f ert-run-tests-batch-and-exit

# Convert Mermaid files to PNG
mermaid: $(PNG_FILES)

images/%.png: diagrams/%.mmd
	@mkdir -p images
	mmdc -i $< -o $@ -b transparent

# Download and extract MNIST data
data: $(MNIST_EXTRACTED)

%-ubyte: %-ubyte.gz
	gunzip -k $<

%-ubyte.gz:
	wget $(MNIST_BASE_URL)/$@
