FROM fpco/stack-build:lts-3.8

RUN mkdir /build
ADD . /build/
WORKDIR /build

RUN stack setup && stack build --copy-bins

