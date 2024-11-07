#!/bin/bash
docker build -t h42n42-image .
docker run -p 8080:8080 -it h42n42-image bash
