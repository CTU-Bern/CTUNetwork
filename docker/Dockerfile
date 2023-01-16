# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    gfortran-7

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
# start in at level above package
## app folder
COPY ./inst/shinyApp ./app
COPY ./ ./CTUNetwork

# making the backend data file accessible
COPY ./TEMP/pf_tabs.rds /app/

# install deps
RUN Rscript -e 'install.packages(c("remotes", "igraph"))'
RUN Rscript -e 'remotes::install_deps("CTUNetwork", dependencies = TRUE)'
RUN Rscript -e 'remotes::install_local("CTUNetwork")'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]