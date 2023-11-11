default: all

all: test build

test: machine assembler checker parser tokenizer

machine:
	runhaskell 01-machine.hs

assembler:
	runhaskell 02-assembler.hs

checker:
	runhaskell 03-checker.hs

parser:
	runhaskell 04-parser.hs

tokenizer:
	runhaskell 05-tokenizer.hs

build:
	ghc 06-jit-compiler.hs -o jitc
