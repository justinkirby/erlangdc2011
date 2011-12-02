#!/bin/bash

curl -s -v -X GET http://localhost:8000/speakers/Francesco%20Cesarini/|python -mjson.tool
