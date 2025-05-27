FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

# Install basic packages
RUN apt-get update && apt-get install -y \
    emacs \
    git \
    curl \
    make \
    && rm -rf /var/lib/apt/lists/*

# Create runner user (like GitHub Actions)
RUN useradd -m runner
USER runner

# Set working directory
WORKDIR /home/runner/work/project

# Copy project files
COPY --chown=runner:runner . .

# Ensure scripts are executable
RUN chmod +x run-tests.sh run_tests_elisp.sh || true

# Run tests
CMD ["./run-tests.sh"]
