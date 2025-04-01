##!/bin/bash

find . -name '*.gz' -exec gunzip '{}' ';'
