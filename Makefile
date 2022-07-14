PROJECT := mattermost

NAME := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
DOCKER_IMAGE := "gerbil/alpine"

$(info "name is " $(NAME))
$(eval uid := $(shell id -u))
$(eval gid := $(shell id -g))

default: linux-static-docker

deps:
	$(GERBIL_HOME)/bin/gxpkg install github.com/ober/oberlib
	$(GERBIL_HOME)/bin/gxpkg install github.com/yanndegat/colorstring

build: deps
	$(GERBIL_HOME)/bin/gxpkg link $(PROJECT) /src || true
	$(GERBIL_HOME)/bin/gxpkg build $(PROJECT)

linux-static-docker:
	docker run -it \
	-e GERBIL_PATH=/src/.gerbil \
	-u "$(uid):$(gid)" \
    -v $(PWD):/src \
	$(DOCKER_IMAGE) \
	make -C /src linux-static

linux-static: build
	/usr/bin/time -avp $(GERBIL_HOME)/bin/gxc -o $(PROJECT)-bin \
	-static \
	-cc-options "-Bstatic" \
	-g -gsrc -genv \
	-ld-options "-static -lpthread -L/usr/lib/x86_64-linux-gnu -lssl -ldl -lyaml -lz " \
	-exe $(PROJECT)/$(PROJECT).ss

clean:
	rm -Rf $(PROJECT)-bin

install:
	mv $(PROJECT)-bin /usr/local/bin/$(PROJECT)
