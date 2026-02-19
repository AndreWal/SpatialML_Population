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

ENV RENV_CONFIG_CACHE_ENABLED=FALSE

# Redirect the "CRAN" repo to PPM's Ubuntu-noble binary mirror.
# renv.lock tags most packages as Repository: CRAN; pointing CRAN at PPM
# means those are installed as pre-built Linux binaries instead of being
# compiled from source, which avoids transient CRAN mirror failures and
# is significantly faster.  Raise the download timeout for large packages.
RUN echo 'options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/noble/latest"), timeout = 300)' \
    >> /usr/local/lib/R/etc/Rprofile.site

# Install CatBoost from the official GitHub Linux binary release.
# This is done as a separate layer before renv::restore() because:
#   - catboost is not on CRAN/PPM (no binary mirror available)
#   - the archive is ~130 MB and benefits from its own retry logic
#   - once installed here, renv::restore() recognises it and skips reinstall
ARG CATBOOST_VERSION=1.2.9
RUN for i in 1 2 3; do \
        curl -fsSL -o /tmp/catboost.tgz \
          "https://github.com/catboost/catboost/releases/download/v${CATBOOST_VERSION}/catboost-R-Linux-${CATBOOST_VERSION}.tgz" \
        && R CMD INSTALL /tmp/catboost.tgz \
        && rm /tmp/catboost.tgz \
        && break; \
        echo "catboost download attempt $i failed, retrying..." >&2; \
        sleep 15; \
    done

# Restore R packages from lockfile.
# Retries up to 5 times with back-off to handle transient download errors;
# already-installed packages are skipped on each subsequent attempt.
RUN for i in 1 2 3 4 5; do \
        R -q -e "renv::restore(prompt = FALSE)" && break; \
        echo "renv::restore attempt $i failed, retrying in 20 s..." >&2; \
        sleep 20; \
    done

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

# Validate that packages restore correctly
RUN for i in 1 2 3 4 5; do \
        R -q -e "renv::restore(prompt = FALSE)" && break; \
        echo "renv::restore attempt $i failed, retrying in 20 s..." >&2; \
        sleep 20; \
    done \
    && R -q -e "library(sf); library(terra); library(arrow); library(duckdb); library(targets); cat('All packages OK\n')"

CMD ["R", "-q", "-e", "targets::tar_make()"]
