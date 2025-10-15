@echo off
docker compose cp SAP_Extractor\K900157.A4H sap:/usr/sap/trans/cofiles
docker compose cp SAP_Extractor\R900157.A4H sap:/usr/sap/trans/data
REM docker compose exec