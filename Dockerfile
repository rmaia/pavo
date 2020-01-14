FROM rocker/tidyverse:devel

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libmagic-dev \
    libmagick++-dev \
    imagemagick \
    libgl1-mesa-dev \
    zlib1g-dev \
    pandoc pandoc-citeproc \
    libglu1-mesa-dev \
    libpng-dev
