#!/bin/bash -e
packages=(
    r-base
    r-base-dev
    r-cran-littler
    libcurl4-openssl-dev
)
if ! dpkg-query -f '.' -W "${packages[@]}" &>/dev/null; then
    sudo apt-get update
    sudo apt-get install --assume-yes "${packages[@]}"
fi
R -e 'print(R.version)'
Rscript ./install.R
r -e 'print("Ready")'

