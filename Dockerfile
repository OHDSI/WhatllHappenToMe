# Set base image
FROM ohdsi/broadsea-shiny:1.0.0

# install additional required OS dependencies
RUN apt-get update && \
    apt-get install -y python3-pip && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# install additional required Python packages
RUN pip install faicons

# install additional required R packages
RUN R -e "remotes::install_github('OHDSI/ResultModelManager@v0.4.0')"
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('shinyDashboard')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('DatabaseConnector')"
RUN R -e "install.packages('shinyWidgets')"
RUN R -e "install.packages('tippy')"
RUN R -e "install.packages('reactable')"
RUN R -e "install.packages('pool')"

# download the jdbc driver
RUN R -e "DatabaseConnector::downloadJdbcDrivers('postgresql', pathToDriver='/root')"

# Set an argument for the app name
ARG APP_NAME

# Set arguments for the GitHub branch and commit id abbreviation
ARG GIT_BRANCH=unknown
ARG GIT_COMMIT_ID_ABBREV=unknown

# Set workdir and copy app files
WORKDIR /srv/shiny-server/${APP_NAME}
COPY . .

# Expose default Shiny app port
EXPOSE 3838

# Run the Shiny app
CMD R -e "shiny::runApp('/srv/shiny-server/${APP_NAME}', host = '0.0.0.0', port = 3838)"
