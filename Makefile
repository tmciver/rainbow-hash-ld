.PHONY: build build-static docker

build:
	nix-build

static/file-ontology.html: doc/file-ontology.ttl
	nix-shell --run "pylode -i $< -o $@"

docker: static/file-ontology.html
	nix-build docker.nix
	docker load < result
