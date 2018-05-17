package = badgers

stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

build:
	$(stack) build $(package)

build-dirty:
	$(stack) build --force-dirty $(package)

build-profile:
	$(stack) --work-dir .stack-work-profiling --profile build

run:
	$(stack) build --fast && $(stack) exec -- $(package)

install:
	$(stack) install

ghci:
	$(stack) ghci $(package):lib

test:
	$(stack) test $(package)

test-ghci:
	$(stack) ghci $(package):test:$(package)-tests

bench:
	$(stack) bench $(package)

ghcid:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --test --ghci-options='-fobject-code -fno-warn-unused-do-bind' --main-is $(package):$(package)"

dev-deps:
	stack install ghcid

reset-database: destroy-create-db migration fixtures

reset-data: truncate-tables fixtures

destroy-create-db:
	-sudo -u postgres dropdb boutique_dev
	sudo -u postgres createdb -O postgres boutique_dev

migration: build
	stack exec -- migration

fixtures: build
	stack exec -- fixtures

truncate-tables: build
	stack exec -- truncate

.PHONY : build build-dirty run install ghci test test-ghci ghcid dev-deps

