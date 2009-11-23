BIN      = bin
BUILD    = build
DOC      = doc
SRC      = src
TMP      = tmp

UNPIG    = $(BIN)/unpig

HC       = ghc
HFLAGS   = -W -Wall

SRCS = $(SRC)/Languages.hs $(SRC)/CommentRemoval.hs $(SRC)/Main.hs

test: unpig
	./bin/test.sh

unpig:
	cd src && hmake -q Main && mv Main ../$(UNPIG) && \
        mv *.hi *.o ../$(BUILD)/

tags:
	hasktags -e $(SRCS)

clean:
	rm -f $(BUILD)/*.o $(BUILD)/*.hi $(TMP)/*.test* $(UNPIG)

help:
	echo "usage: make [unpig | test | tags | clean | help]"
