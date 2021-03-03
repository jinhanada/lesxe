SRC = ./src
OUT = ./out
BIN = ./bin
SRCS = $(SRC)/lesxe.c $(SRC)/lesxe.h


.PHONY: all
all: $(BIN)/lesxe

$(OUT)/lesxe.o: $(SRC)/lesxe.c $(SRC)/lesxe.h
	$(CC) $(CFLAGS) -c -o $(OUT)/lesxe.o $(SRC)/lesxe.c

$(BIN)/lesxe: $(OUT)/lesxe.o $(SRC)/main.c
	$(CC) $(CFLAGS) -o $(BIN)/lesxe $(SRC)/main.c $(OUT)/lesxe.o

$(OUT)/test_lesxe: $(OUT)/lesxe.o $(SRC)/test.c
	$(CC) $(CFLAGS) -o $(OUT)/test_lesxe $(SRC)/test.c

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

