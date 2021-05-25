FROM haskell:9.0.1-stretch as base

WORKDIR /tmp/elka-life

# copy package configuration, install compiler and dependencies (if any)
COPY ./stack.yaml ./stack.yaml.lock ./
COPY ./elka-life/package.yaml ./elka-life/
RUN stack build --dependencies-only

# copy package source and install package
COPY ./elka-life/src/ ./elka-life/src/
COPY ./elka-life/app/ ./elka-life/app/
RUN stack build \
    && mv "$(stack path --local-install-root)/bin" ./bin

# copy binary only to smaller image (we need glibc, because it's linked, so ubuntu is used)
FROM ubuntu:21.04

WORKDIR /tmp/elka-life

COPY --from=base /tmp/elka-life/bin/ ./

ENTRYPOINT ["./elka-life"]
