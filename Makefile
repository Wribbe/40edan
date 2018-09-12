DIR_BIN := bin
DIR_OBJ := obj
DIR_HI := hi

NON_BIN := Utilities.hs Chatterbot.hs
NON_BIN += $(wildcard old_*)

DIRS := $(DIR_BIN) $(DIR_OBJ) $(DIR_HI)

BINS := $(patsubst %.hs,$(DIR_BIN)/%,$(filter-out $(NON_BIN), $(wildcard *.hs)))


all: $(BINS) crypt/.sentinel

$(DIR_BIN)/% : Chatterbot.hs %.hs | $(DIRS)
	ghc -o $@ -odir $(DIR_OBJ) -hidir $(DIR_HI) -dynamic $(filter-out $(NON_BIN),$^)
	@rm -rf $(DIR_OBJ)/*

$(DIRS) :
	@mkdir $@

% :: %.tar.gz.gpg
	@gpg -o decrypted -d $^
	@tar xzf decrypted $@
	@rm decrypted

.PRECIOUS: % %/.sentinel
%/.sentinel : | %
	touch $@
