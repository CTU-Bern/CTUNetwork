# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# System libraries of general use
## Install debian packages
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
    libblas-dev \
    liblapack-dev \
    libarpack2-dev \
    libgmp-dev \ 
    libglpk-dev \
    git \
    cmake \
    gcc \
    unixodbc \
    odbcinst \
    unixodbc-dev

## Update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

## Cloning the MariaDB Connector/ODBC Git Repository
RUN git clone https://github.com/MariaDB/mariadb-connector-odbc.git
RUN WORKDIR "mariadb-connector-odbc"

## Build MariaDB Connector/ODBC from the git repository
RUN cmake -DCMAKE_BUILD_TYPE=RelWithDebInfo -DCONC_WITH_UNIT_TESTS=Off -DCONC_WITH_MSI=OFF -DCMAKE_INSTALL_PREFIX=/usr/local .
RUN cmake --build . --config RelWithDebInfo

## Installing MariaDB Connector/ODBC
RUN sudo make install

## Navigate to the previous directory
RUN cd -

# Copy necessary files
# start in at level above package
## app folder
COPY ./inst/shinyApp ./app
COPY ./ ./CTUNetwork

# Making the backend data file accessible
COPY ./TEMP/pf_tabs.rds /app/

# Install deps
RUN Rscript -e 'install.packages(c("remotes", "igraph"))'
RUN Rscript -e 'remotes::install_deps("CTUNetwork", dependencies = TRUE)'
RUN Rscript -e 'remotes::install_local("CTUNetwork")'

# Expose port
EXPOSE 3838

# Run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]