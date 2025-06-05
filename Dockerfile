# Build stage
FROM haskell:9.10 AS builder

RUN apt-get clean && \
    rm -rf /var/lib/apt/lists/* && \
    apt-get update --allow-releaseinfo-change && \
    apt-get install -y \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

RUN cabal update

# Copy and build
WORKDIR /app
COPY haskell-mcp-server.cabal /app
COPY CHANGELOG.md /app
COPY LICENSE /app
COPY SPEC.md /app

RUN cabal build all --only-dependencies

COPY src /app/src
COPY app /app/app
COPY examples /app/examples
COPY test /app/test

RUN cabal build all

# Install all executables to /usr/local/bin
RUN cabal install exe:haskell-mcp-server --installdir=/usr/local/bin
RUN cabal install exe:high-level-example --installdir=/usr/local/bin
RUN cabal install exe:template-haskell-example --installdir=/usr/local/bin

# Runtime stage
FROM debian:bullseye-slim

RUN apt-get update && \
    apt-get install -y \
    ca-certificates \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

# Copy all binaries from builder
COPY --from=builder /usr/local/bin/haskell-mcp-lib /usr/local/bin/haskell-mcp-lib
COPY --from=builder /usr/local/bin/high-level-example /usr/local/bin/high-level-example
COPY --from=builder /usr/local/bin/template-haskell-example /usr/local/bin/template-haskell-example

WORKDIR /app

# Default entrypoint is the main executable
ENTRYPOINT ["/usr/local/bin/haskell-mcp-lib"]