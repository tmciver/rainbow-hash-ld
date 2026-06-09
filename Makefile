.PHONY: build build-static docker

build:
	nix-build

build-static:
	nix-build static.nix

docker:
	nix-build docker.nix
	docker load < result
