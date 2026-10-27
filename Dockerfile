# syntax=docker/dockerfile:1
FROM rocker/geospatial:4.5.0
ARG RENV_PATHS_CACHE=/root/.cache/R/renv
ENV "RENV_PATHS_CACHE"="${RENV_PATHS_CACHE}"
# Lockfile pins source-only cran.rstudio.com; P3M serves Ubuntu noble binaries
ENV RENV_CONFIG_REPOS_OVERRIDE="https://p3m.dev/cran/__linux__/noble/latest"
RUN apt-get update -y && apt-get install -y  libicu-dev cmake make libuv1-dev libx11-dev libcurl4-openssl-dev libssl-dev zlib1g-dev pandoc libfontconfig1-dev libfreetype6-dev libfribidi-dev libharfbuzz-dev libjpeg-dev libpng-dev libtiff-dev libwebp-dev git libxml2-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = FALSE, download.file.method = 'libcurl', Ncpus = 4)" | tee -a /usr/local/lib/R/etc/Rprofile.site | tee -a /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_version("renv", version = "1.1.4")'
COPY renv.lock renv.lock
RUN --mount=type=cache,id=renv-cache,target=${RENV_PATHS_CACHE} R -e 'renv::restore()'
WORKDIR /srv/shiny-server/
COPY . /srv/shiny-server/
EXPOSE 8080
CMD R -e 'shiny::runApp("/srv/shiny-server",host="0.0.0.0",port=8080)'
