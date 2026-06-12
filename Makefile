.PHONY: build build-static docker

build:
	nix-build

docker:
	nix-build docker.nix
	docker load < result
