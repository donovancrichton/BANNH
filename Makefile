PPATH:=$(shell pwd)
TARGET1=BANNHL
TARGET2=BANNHC
TARGET3=report
CC=ghc
CFLAGS=-O2 -o
DC=latexmk
DFLAGS=-pdf -f -shell-escape

.PHONY: docs

all: bannh docs

bannh:
	@cd $(PPATH)/src/; $(CC) $(CFLAGS) $(TARGET1) $(TARGET1).hs
	@cd $(PPATH)/src/; $(CC) $(CFLAGS) $(TARGET2) $(TARGET2).hs
	#remove Haskell build objects from src
	@mkdir -p $(PPATH)/bin
	@mv $(PPATH)/src/$(TARGET1) $(PPATH)/bin
	@mv $(PPATH)/src/$(TARGET2) $(PPATH)/bin
	@-rm -f $(PPATH)/src/*.o
	@-rm -f $(PPATH)/src/*.hi

docs:
	@cd $(PPATH)/src/; $(DC) $(DFLAGS) $(TARGET3).tex
	@cd $(PPATH)/src/; $(DC) -c $(TARGET3).tex
	#remove minted report folder
	@rm -rf $(PPATH)/src/_minted-report
	#remove any unwanted backups
	@rm -f $(PPATH)/src/report.tex.bak
	@mkdir -p $(PPATH)/docs
	@mv $(PPATH)/src/$(TARGET3).pdf $(PPATH)/docs
  
clean:
	@rm -f $(PPATH)/bin/$(TARGET1)
	@rm -f $(PPATH)/bin/$(TARGET2)
	@rm -f $(PPATH)/docs/$(TARGET3).pdf

