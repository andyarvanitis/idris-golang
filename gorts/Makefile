include ../config.mk

.PHONY: build install test

GOPATH := ${GOPATH}:${PWD}:${PWD}/gorts

ifdef TARGET
INSTALLGOSTRUCTURE=rsync -rupE src pkg bin $(TARGET)
endif


install:
	go install idris_runtime
	$(INSTALLGOSTRUCTURE)

build:
	go build idris_runtime

clean:
	go clean idris_runtime
