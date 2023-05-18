# # GNU Make 4.2.1
CC := gcc
SRCDIR := src
BUILDDIR := build
TARGET := bin/mycc
TARGETDIR := bin
SRCEXT := c
SOURCES := $(shell find $(SRCDIR) -type f -name *.$(SRCEXT))
OBJECTS := $(patsubst $(SRCDIR)/%,$(BUILDDIR)/%,$(SOURCES:.$(SRCEXT)=.o))
CFLAGS := 
LIB := -L lib
INC := -I include

$(TARGET): $(OBJECTS)
	@echo " Linking..."; 
	@mkdir -p $(TARGETDIR);
	@echo " $(CC) $^ -o $(TARGET) $(LIB)"; $(CC) $^ -o $(TARGET) $(LIB)

$(BUILDDIR)/%.o: $(SRCDIR)/%.$(SRCEXT)
	@echo " Building..."
	@mkdir -p $(BUILDDIR)
	@echo " $(CC) $(CFLAGS) $(INC) -c -o $@ $<"; $(CC) $(CFLAGS) $(INC) -c -o $@ $<

test: $(TARGET)
	@echo " Testing..."
	@echo " sh test/test.sh"; sh test/test.sh

clean: test
	@echo " Cleaning..."; 
	@echo " $(RM) -r $(BUILDDIR) $(TARGETDIR)"; $(RM) -r $(BUILDDIR) $(TARGETDIR); 
	@echo " rm -rf `find test/* | egrep -v '(test.sh|*.txt)'`"; rm `find test/* | egrep -v '(test.sh|*.txt)'`;
	@clear

.PHONY: clean