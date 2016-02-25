PROGRAM_NAME=GearBot-irc

.PHONY: build
build:
	stack build

.PHONY: install
install: build
	stack install

.PHONY: clean
clean:
	stack clean

.PHONY: run
run: install
	stack exec $(PROGRAM_NAME)

.PHONY: watch
watch: clean
	stack build --file-watch
