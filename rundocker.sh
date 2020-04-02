#!/bin/bash

echo Launching RStudio in browser now.... 
echo Login with username: rstudio and password: fischer-rrr
sudo docker run --rm -v `pwd`:/home/rstudio/manuscript -e PASSWORD="fischer-rrr" -p 8787:8787 lcolling/fischer-docker
