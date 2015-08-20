#!/bin/sh

git pull
stack build
sudo supervisorctl restart mat
