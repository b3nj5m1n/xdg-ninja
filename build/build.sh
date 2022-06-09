#!/bin/env sh
docker build -t xdgnj .  || exit 1
docker run --rm --volume "$(pwd):/data" xdgnj cp "/root/.local/bin/xdgnj" "/data/xdgnj"  || exit 1
sudo chown "$USER" xdgnj
