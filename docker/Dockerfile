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
    libblas-dev \
    liblapack-dev \
    libarpack2-dev \
    libgmp-dev \ 
    libglpk-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

## install MariaDB Connector/ODBC
RUN mkdir odbc_package
RUN cd odbc_package
RUN wget https://downloads.mariadb.com/Connectors/odbc/connector-odbc-3.1.7/mariadb-connector-odbc-3.1.7-ga-debian-x86_64.tar.gz
RUN tar -xvzf mariadb-connector-odbc-3.1.7-ga-debian-x86_64.tar.gz
RUN sudo install lib64/libmaodbc.so /usr/lib/
RUN sudo install -d /usr/lib/mariadb/ \
    /usr/lib/mariadb/plugin/
RUN sudo install lib/mariadb/plugin/auth_gssapi_client.so /usr/lib/mariadb/plugin/ \
    lib/mariadb/plugin/caching_sha2_password.so /usr/lib/mariadb/plugin/
    lib/mariadb/plugin/client_ed25519.so /usr/lib/mariadb/plugin/
    lib/mariadb/plugin/dialog.so /usr/lib/mariadb/plugin/
    lib/mariadb/plugin/mysql_clear_password.so /usr/lib/mariadb/plugin/
    lib/mariadb/plugin/sha256_password.so /usr/lib/mariadb/plugin/
RUN cd -

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