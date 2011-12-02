#!/bin/bash

curl -s -v -X GET http://localhost:8000/speakers/|python -mjson.tool
