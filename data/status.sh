#!/bin/bash

curl -v http://localhost:8000/status/$@ | python -mjson.tool
