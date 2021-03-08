.PHONY: default
default: sc ;

sc:
	docker buildx build \
		-f Dockerfile.session-counter \
		-t jadudm/arm64-session-counter \
		--platform linux/arm64/v8 \
		--output "type=docker,push=false,name=jadudm/session-counter-arm64v8,dest=session-counter-arm64.tar" \
		.

racket:
	rm -f racket8-arm64.tar
	docker buildx build \
		-f Dockerfile.racket \
		-t jadudm/racket8-arm64v8 \
		--platform linux/arm64/v8 \
		--output "type=docker,push=false,name=jadudm/racket8-arm64v8,dest=racket8-arm64.tar" \
		.
	# docker rm jadudm/arm64-racket8
	# docker rmi $(docker images 'jadudm/arm64-racket8' -a -q)
	# docker load --input racket8-arm64.tar

racketpi:
	docker build \
		-f Dockerfile.racket \
		-t jadudm/racket8-arm64v8 \
		--output "type=docker,push=false,name=jadudm/racket8-arm64v8,dest=racket8-arm64.tar" \
		.

all: clean racket sc

clean:
	docker rm jadudm/arm64-racket8
	docker rm jadudm/arm64-session-counter
	docker rmi $(docker images 'jadudm/arm64-racket8' -a -q)
	docker rmi $(docker images 'jadudm/arm64-session-counter' -a -q)
