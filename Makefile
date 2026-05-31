.PHONY: build build-static docker

build:
	nix-build

build-static:
	nix-build static.nix

docker: build-static
	mkdir -p ./docker
	rm -f ./docker/caldron-server
	cp result/bin/caldron-server docker/
	docker build -t com.timmciver/caldron:latest .
