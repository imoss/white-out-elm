#!/bin/sh

watchman-make -p 'src/**/*.elm' --run 'npm run build'
