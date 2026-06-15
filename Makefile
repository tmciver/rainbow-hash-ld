.PHONY: build build-static docker ontology-docs

build:
	nix-build

ontology-docs: static/file-ontology.html

static/file-ontology.html: doc/file-ontology.ttl
	nix-shell --run "pylode -i $< -o $@"

docker: static/file-ontology.html
	nix-build docker.nix
	docker load < result
