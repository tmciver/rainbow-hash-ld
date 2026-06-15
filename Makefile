.PHONY: build build-static docker ontology-docs

build:
	nix-build

ontology-docs:
	pylode -i doc/file-ontology.ttl -o static/file-ontology.html

docker:
	nix-build docker.nix
	docker load < result
