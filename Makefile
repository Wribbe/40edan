DIR_BIN := bin
DIR_OBJ := obj
DIR_HI := hi

NON_BIN := Utilities.hs Chatterbot.hs

DIRS := $(DIR_BIN) $(DIR_OBJ) $(DIR_HI)

BINS := $(patsubst %.hs,$(DIR_BIN)/%,$(filter-out $(NON_BIN), $(wildcard *.hs)))

all: $(BINS)

$(DIR_BIN)/% : %.hs | $(DIRS)
	ghc -o $@ -odir $(DIR_OBJ) -hidir $(DIR_HI) -dynamic $^
	@rm $(DIR_OBJ)/Main.o

$(DIRS) :
	@mkdir $@
