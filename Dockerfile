FROM haskell:9.6.7-slim AS builder

WORKDIR /build

# Layer 1: dependency cache (only invalidated when the .cabal file changes)
COPY palisade.cabal cabal.project ./
RUN cabal update && \
    cabal build --only-dependencies --disable-tests -j$(nproc) 2>&1 | tail -n 20

# Layer 2: full source
COPY . .
RUN cabal build --disable-tests -j$(nproc) exe:palisade && \
    cp "$(cabal list-bin palisade)" /out-palisade

# ── Stage 2: runtime ──────────────────────────────────────────────────────
FROM debian:bookworm-slim AS runtime

# gmp + libffi are the only Haskell RTS deps needed at runtime
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        libgmp10 \
        libffi8 \
        ca-certificates && \
    rm -rf /var/lib/apt/lists/*

COPY --from=builder /out-palisade /usr/local/bin/palisade
COPY scenarios /usr/share/palisade/scenarios
RUN chmod +x /usr/local/bin/palisade

LABEL org.opencontainers.image.title="palisade" \
      org.opencontainers.image.description="A proof-gated execution kernel for machine-initiated money" \
      org.opencontainers.image.authors="Sharif Parish <saintparish6@gmail.com>" \
      org.opencontainers.image.source="https://github.com/saintparish4/palisade" \
      org.opencontainers.image.licenses="MIT"

# Mount a directory with your policies, logs and intents:
#   docker run --rm -v $(pwd):/data ghcr.io/saintparish4/palisade check /data/policy.pal
# The example scenarios ship in the image:
#   docker run --rm ghcr.io/saintparish4/palisade bench -p /usr/share/palisade/scenarios/policies/hardened.pal /usr/share/palisade/scenarios/policybench
VOLUME ["/data"]
WORKDIR /data

ENTRYPOINT ["/usr/local/bin/palisade"]
CMD ["--help"]
