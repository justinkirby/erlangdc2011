#!/bin/bash

curl -s -v -X DELETE http://localhost:8000/speakers/Francesco%20Cesarini/|python -mjson.tool
