SRC = ./src
OUT = ./out
BIN = ./bin
SRCS = $(SRC)/lesxe.c $(SRC)/lesxe.h


.PHONY: all
all: $(BIN)/lesxe $(BIN)/text2c

$(SRC)/corelib.h: $(BIN)/text2c $(SRC)/core.le
	cat $(SRC)/core.le | $(BIN)/text2c corelib_src > $(SRC)/corelib.h

$(OUT)/lesxe.o: $(SRC)/lesxe.c $(SRC)/lesxe.h $(SRC)/corelib.h
	$(CC) $(CFLAGS) -c -o $(OUT)/lesxe.o $(SRC)/lesxe.c

$(BIN)/lesxe: $(OUT)/lesxe.o $(SRC)/main.c
	$(CC) $(CFLAGS) -o $(BIN)/lesxe $(SRC)/main.c $(OUT)/lesxe.o

$(OUT)/test_lesxe: $(OUT)/lesxe.o $(SRC)/test.c
	$(CC) $(CFLAGS) -o $(OUT)/test_lesxe $(SRC)/test.c

$(BIN)/text2c: $(SRC)/text2c.c
	$(CC) $(CFLAGS) -o $(BIN)/text2c $(SRC)/text2c.c

.PHONY: test
test: all $(OUT)/test_lesxe
	./out/test_lesxe


.PHONY: clean
clean:
	rm -f $(OUT)/* $(BIN)/lesxe


.PHONY: memcheck
memcheck: all test
	valgrind \
		--tool=memcheck \
		--leak-check=full \
		--log-file=out/valgrind.log \
		./out/test_lesxe

