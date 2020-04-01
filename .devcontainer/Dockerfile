#-------------------------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See https://go.microsoft.com/fwlink/?linkid=2090316 for license information.
#-------------------------------------------------------------------------------------------------------------

FROM rocker/r-apt:bionic

# Avoid warnings by switching to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

# The r-apt image comes with a base non-root 'docker' user which this Dockerfile
# gives sudo access. However, for Linux, this user's GID/UID must match your local
# user UID/GID to avoid permission issues with bind mounts. Update USER_UID / USER_GID 
# if yours is not 1000. See https://aka.ms/vscode-remote/containers/non-root-user.
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# Configure apt and install packages
RUN apt-get update \
    && apt-get -y install --no-install-recommends apt-utils dialog 2>&1 \
    #
    # install git iproute2, process tools, lsb-release (common in install instructions for CLIs) and libzip for R Tools extension
    && apt-get -y install git openssh-client iproute2 procps lsb-release libzip-dev \
    #
    # Register Microsoft key and feed
    && wget -q https://packages.microsoft.com/config/ubuntu/18.04/packages-microsoft-prod.deb \
    && dpkg -i packages-microsoft-prod.deb \
    #
    # Install the .NET Runtime
    && add-apt-repository universe \
    && apt-get install apt-transport-https \
    && apt-get update \
    && apt-get -y install dotnet-runtime-2.1 \
    #
    # [Optional] Update a non-root user to match UID/GID - see https://aka.ms/vscode-remote/containers/non-root-user.
    && if [ "$USER_GID" != "1000" ]; then groupmod docker --gid $USER_GID; fi \
    && if [ "$USER_UID" != "1000" ]; then usermod --uid $USER_UID docker; fi \
    # [Optional] Add sudo support for the non-root user
    && apt-get install -y sudo \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/docker\
    && chmod 0440 /etc/sudoers.d/docker \
    #
    # Clean up
    && apt-get autoremove -y \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/* 
    
# Script with necessary setup
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        libxml2-dev \
        #libcairo2-dev \
        #libpq-dev \
        #libssl-dev \
        #libcurl4-openssl-dev \
        #libssh2-1-dev \
        ghostscript \
        lmodern \
        pandoc-citeproc \
        qpdf \
        r-cran-formatr \
        r-cran-ggplot2 \
        r-cran-knitr \
		r-cran-rmarkdown \
        r-cran-runit \
        r-cran-testthat \
        texinfo \
        texlive-fonts-extra \
        texlive-fonts-recommended \
        texlive-latex-extra \
        texlive-latex-recommended \
        texlive-luatex \
        texlive-plain-generic \
        texlive-science \
        texlive-xetex \
    && install.r binb linl pinp tint \
    && mkdir ~/.R \
        && echo _R_CHECK_FORCE_SUGGESTS_=FALSE > ~/.R/check.Renviron \
        && cd /usr/local/bin \
        && ln -s /usr/lib/R/site-library/littler/examples/render.r .

RUN R -e "install.packages(c('tidyverse','dplyr','tinytex'),repos='http://cran.rstudio.com/')"

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=dialogDockerfile

                            # 'gridExtra',
                            # 'ggiraph',
                            # 'ggiraphExtra',
                            # 'waffle',
                            # 'scales',
                            # 'devtools')