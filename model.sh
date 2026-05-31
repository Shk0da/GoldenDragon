#!/bin/bash
cd /home/apps/dragon/ &&
mkdir -p "logs"
date=$(date '+%Y-%m-%d')
java -Dapplication.properties=./application.properties -jar GoldenDragon-1.0.jar GenerateModel --data=ml_strategy/data_pipeline/trades.csv --output=models/trade_classifier_v2.txt --report-dir=ml_strategy > logs/"${date}_GenerateModel.log"