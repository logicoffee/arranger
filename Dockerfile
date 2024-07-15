FROM nixos/nix:latest AS builder

COPY . /tmp/build
WORKDIR /tmp/build

RUN nix \
    --extra-experimental-features "nix-command flakes" \
    --option filter-syscalls false \
    build

RUN mkdir /tmp/nix-store
RUN cp -R $(nix-store -qR result/) /tmp/nix-store


FROM ubuntu:24.10

RUN apt-get -yq update && \
    apt-get -yq --no-install-suggests --no-install-recommends install \
    ca-certificates

WORKDIR /app

EXPOSE 8000
COPY --from=builder /tmp/nix-store /nix/store
COPY --from=builder /tmp/build/result /app
CMD ["/app/bin/arranger"]
