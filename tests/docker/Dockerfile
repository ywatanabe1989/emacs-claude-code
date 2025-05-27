# Dockerfile to mimic GitHub Actions environment
FROM ubuntu:22.04

# Prevent interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install basic dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    git \
    software-properties-common \
    && rm -rf /var/lib/apt/lists/*

# Install multiple Emacs versions (mimicking purcell/setup-emacs)
RUN add-apt-repository ppa:kelleyk/emacs && \
    apt-get update && \
    apt-get install -y \
    emacs27 \
    emacs28 \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /workspace

# Copy project files
COPY . /workspace

# Default to Emacs 28
RUN update-alternatives --set emacs /usr/bin/emacs28

# Run tests by default
CMD ["./run-tests.sh"]