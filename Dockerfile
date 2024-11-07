# Use Ubuntu 22.04 as the base image
FROM ubuntu:22.04

# Set environment variables to avoid prompts during installations
ENV DEBIAN_FRONTEND=noninteractive

# Update package list and install system dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential \
    m4 \
    git \
    curl \
    opam \
    libsqlite3-dev \
    pkg-config \
    unzip \
    wget \
    ca-certificates \
	libgmp-dev \
	libssl-dev \
	zlib1g-dev \
    apt-transport-https && \
    rm -rf /var/lib/apt/lists/*

# Install Node.js (adjust the version if needed)
RUN curl -fsSL https://deb.nodesource.com/setup_16.x | bash - && \
    apt-get install -y nodejs

# Initialize opam and create an OCaml switch (version 4.13.1 as an example)
RUN opam init --disable-sandboxing -a && \
    opam switch create 4.13.1 && \
    eval $(opam env) && \
    opam install -y dune ocsipersist-sqlite eliom ocsigen-ppx-rpc

# Set environment variables for opam
ENV OPAMYES=1
ENV OPAMJOBS=2
ENV OPAMVERBOSE=1

# Set the working directory
WORKDIR /app

# Copy only the package files first to leverage Docker cache
COPY package.json package-lock.json /app/

# Install npm dependencies
RUN npm install

# Copy the rest of the project files into the container
COPY . /app

# Install project dependencies via opam
RUN eval $(opam env) && \
    opam install . --deps-only


# Expose ports if your application uses any (adjust as necessary)
EXPOSE 8080

# Set the command to run your application (adjust as necessary)
CMD ["/bin/bash"]

