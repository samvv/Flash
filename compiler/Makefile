
all: src/ast.ts
	bolt check ../stdlib ../test2.bolt
	npm test

src/ast.ts: src/ast-spec.ts
	@echo "Generating AST definitions ..."
	@mkdir -p lib/
	@tsastgen src/ast-spec.ts:src/ast.ts --with-parent-member=parentNode

.PHONY: clean

clean:
	rm -rf src/ast.ts

.PHONY: distclean

distclean:
	rm -rf lib/

