STACK_FLAGS ?=

IVORYLANG_ORG_EXEC = stack build $(STACK_FLAGS) --exec 'ivorylang-org $(1)'

default: build

.PHONY: build preview deploy clean
build:
	$(call IVORYLANG_ORG_EXEC,build)

preview: build
	$(call IVORYLANG_ORG_EXEC,preview)

deploy:
	$(call IVORYLANG_ORG_EXEC,deploy)

clean:
	$(call IVORYLANG_ORG_EXEC,clean)
