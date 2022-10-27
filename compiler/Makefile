
all: src/ast.ts
<<<<<<< HEAD
	npm test
=======
	npx bolt check ../stdlib ../compiler/test.bolt
>>>>>>> 0cfe6c9 (Upgrade dependencies, rewrite README.md and fix some compile errors)

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

