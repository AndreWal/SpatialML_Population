# ---- Stage 1: System deps + R packages ----
FROM rocker/geospatial:4.5.2 AS base

LABEL maintainer="Spatial Predictions Project"
LABEL description="Reproducible multi-country geospatial ETL + spatial ML pipeline"

# System libraries needed by R packages not in rocker/geospatial
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    libgit2-dev \
    libsodium-dev \
    python3-pip \
    python3-venv \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /project

# Copy renv infrastructure first (Docker layer caching)
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R

# Configure renv to use Posit Package Manager for fast binary installs
ENV RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/noble/latest"
ENV RENV_CONFIG_CACHE_ENABLED=FALSE

# Restore R packages from lockfile
RUN R -q -e "renv::restore(prompt = FALSE)"

# ---- Stage 2: Copy project files and validate ----
FROM base AS project

WORKDIR /project

# Copy project structure
COPY config/ config/
COPY R/ R/
COPY tests/ tests/
COPY _targets.R _targets.R
COPY docs/ docs/
COPY AGENTS.md AGENTS.md
COPY CLAUDE.md CLAUDE.md

# Create data directories (raw data is mounted at runtime, not baked in)
RUN mkdir -p data/raw data/intermediate data/final logs models cache

# Default: run pipeline in mock mode (no raw data needed)
ENV MOCK_MODE=true

# Validate that packages restore correctly
RUN R -q -e "renv::restore(prompt = FALSE)" \
    && R -q -e "library(sf); library(terra); library(arrow); library(duckdb); library(targets); cat('All packages OK\n')"

CMD ["R", "-q", "-e", "targets::tar_make()"]
