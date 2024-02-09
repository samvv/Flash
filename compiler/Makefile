
all: src/ast.ts
	npx bolt check ../stdlib ../compiler/test.bolt

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

