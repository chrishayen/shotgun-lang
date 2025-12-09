PREFIX ?= $(HOME)/.local
BUILD_DIR = compiler/build

.PHONY: build test clean install uninstall demo grammar

build:
	@mkdir -p $(BUILD_DIR)
	cd $(BUILD_DIR) && cmake .. && make

test: build
	cd $(BUILD_DIR) && ctest --output-on-failure
	$(BUILD_DIR)/shotgun test

clean:
	rm -rf $(BUILD_DIR)
	rm -f *.c demo

install: build
	mkdir -p $(PREFIX)/bin
	cp $(BUILD_DIR)/shotgun $(PREFIX)/bin/shotgun

uninstall:
	rm -f $(PREFIX)/bin/shotgun

demo: build
	$(BUILD_DIR)/shotgun build examples/demo.bs -o demo
	./demo

grammar:
	cd tree-sitter-shotgun && npx tree-sitter generate
