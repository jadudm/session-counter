all:
	tar xvzf racket-8.0-src.tgz
	docker build \
		--volume racket-8.0:/racket-8.0 \
		--volume ${PWD}:/app \
		-t 18f/session-counter .
