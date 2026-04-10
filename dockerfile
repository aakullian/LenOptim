FROM --platform=linux/amd64 rocker/shiny:4.4
ARG SASTOKEN

RUN apt-get update
RUN apt-get install -y wget libglpk40
RUN apt-get install -y libudunits2-dev libproj25 libgdal-dev \
gdal-bin \
&& rm -rf /var/lib/apt/lists/*

RUN echo $SASTOKEN

# Download azcopy (for pulling data from Azure file share)
RUN wget https://aka.ms/downloadazcopy-v10-linux \
    && tar -xvf downloadazcopy-v10-linux \
    && cp ./azcopy_linux_amd64_*/azcopy /usr/bin/ \
    && rm -frd azcopy_linux_amd64_10.12.2/ \
    && rm downloadazcopy-v10-linux

RUN R -q -e "install.packages('shiny')" && \
    R -q -e "install.packages('conflicted')" && \
    R -q -e "install.packages('tidyr')" && \
    R -q -e "install.packages('dplyr')" && \
    R -q -e "install.packages('stringr')" && \
    R -q -e "install.packages('sf')" && \
    R -q -e "install.packages('ggplot2')" && \
    R -q -e "install.packages('scales')" && \
    R -q -e "install.packages('patchwork')" && \
    R -q -e "install.packages('viridis')" && \
    R -q -e "install.packages('ggrepel')" && \
    R -q -e "install.packages('DT')" && \
    R -q -e "install.packages('purrr')"

RUN echo "$SASTOKEN"

# to download data from Azure storage account using SAS key
RUN azcopy cp --recursive "https://lenoptimdata.file.core.windows.net/?$SASTOKEN" /srv/shiny-server/shiny

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
ADD ./R/shiny_app /srv/shiny-server/shiny

# for debugging
# CMD ["tail", "-f", "/dev/null"]