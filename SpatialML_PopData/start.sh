#! /bin/bash

set -a
source .env
set +a

docker build -t my-geospatial-r:4.5.2 .
docker run --rm -ti \
  -e USER=$USERNAME \
  -e PASSWORD=$PASSWORD \
  -p 8787:8787 \
  -v "$(pwd)":/home/rstudio/project \
  my-geospatial-r:4.5.2