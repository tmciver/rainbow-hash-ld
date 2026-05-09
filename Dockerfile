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

# Create entrypoint script (before switching users)
RUN echo '#!/bin/sh' > /usr/local/bin/entrypoint.sh && \
    echo 'set -e' >> /usr/local/bin/entrypoint.sh && \
    echo 'export HOME=/home/caldron' >> /usr/local/bin/entrypoint.sh && \
    echo 'export XDG_CONFIG_HOME=/home/caldron/.config' >> /usr/local/bin/entrypoint.sh && \
    echo 'echo "DEBUG: HOME=$HOME, XDG_CONFIG_HOME=$XDG_CONFIG_HOME"' >> /usr/local/bin/entrypoint.sh && \
    echo 'exec /usr/local/bin/caldron-server --file-store-url "$FILE_STORE_URL" --sparql-url "$SPARQL_URL"' >> /usr/local/bin/entrypoint.sh && \
    chmod +x /usr/local/bin/entrypoint.sh

# Create a non-root user
RUN groupadd --gid 1000 caldron && \
    useradd --uid 1000 --gid caldron --shell /bin/sh --create-home caldron

# Set XDG config directory
ENV XDG_CONFIG_HOME=/home/caldron/.config

# Create the config directory (after user creation)
RUN mkdir -p "$XDG_CONFIG_HOME" && chown caldron:caldron "$XDG_CONFIG_HOME"

# Switch to non-root user
USER caldron

# Expose the port
EXPOSE 8081

# Set the entrypoint
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
