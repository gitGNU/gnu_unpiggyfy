BIN      = bin
BUILD    = build
DOC      = doc
SRC      = src
TMP      = tmp

UNPIG    = $(BIN)/unpig

HC       = ghc
HC_FLAGS = -W -Wall

SRCS = $(SRC)/CommentRemoval.hs  $(SRC)/Main.hs
OBJS = $(BUILD)/CommentRemoval.o $(BUILD)/Main.o

unpig: $(OBJS)
	rm -f $(UNPIG)
	$(HC) $(HC_FLAGS) -o $(UNPIG) $(OBJS)

$(BUILD)/CommentRemoval.o:
	$(HC) $(HC_FLAGS) -hidir $(BUILD) -odir $(BUILD) \
              -c $(SRC)/CommentRemoval.hs

$(BUILD)/Main.o:
	$(HC) $(HC_FLAGS) -hidir $(BUILD) -odir $(BUILD) -i$(BUILD) \
              -c $(SRC)/Main.hs

tags:
	hasktags -e $(SRCS)

clean:
	rm -f $(BUILD)/*.o $(BUILD)/*.hi $(UNPIG)
