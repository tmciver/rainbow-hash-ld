# Use a minimal Debian base image
FROM debian:12-slim

# Install only the essential runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

# Copy the compiled binary
COPY caldron-server /usr/local/bin/caldron-server

# Make it executable
RUN chmod +x /usr/local/bin/caldron-server

# Create a non-root user
RUN groupadd --gid 1000 caldron && \
    useradd --uid 1000 --gid caldron --shell /bin/sh --create-home caldron

# Switch to non-root user
USER caldron

# Expose the port
EXPOSE 8081

# Set the entrypoint with shell form for environment variable substitution
ENTRYPOINT /usr/local/bin/caldron-server --file-store-url "$FILE_STORE_URL" --sparql-url "$SPARQL_URL"