FILENAME = Main

.DEFAULT_GOAL := compile-run

compile-run:
	@make compile
	@make run

compile:
	ghc ${FILENAME}.hs

run:
	./${FILENAME}

clean:
	@rm ./*.o && rm ./*.hi || true