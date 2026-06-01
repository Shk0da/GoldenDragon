#!/bin/bash
cd /home/apps/dragon/ &&
mkdir -p "logs"
date=$(date '+%Y-%m-%d')
java -Dapplication.properties=./application.properties -jar GoldenDragon-1.0.jar GenerateModel > logs/"${date}_GenerateModel.log"