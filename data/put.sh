#!/bin/bash
curl -s -v -X PUT -H "Content-Type: application/json" http://localhost:8000/speakers/Jeltz --data @../data/vogon.json
