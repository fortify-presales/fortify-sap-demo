FROM sapse/abap-cloud-developer-trial:2023

# Add license files
#COPY HDB.txt /opt/sap/HDB_license
#COPY A4H_Multiple.txt /opt/sap/ASABAP_license

# Copy SAP Extractor transport files
#COPY SAP_Extractor/K*.* /usr/sap/trans/cofiles
#RUN chmod 777 /usr/sap/trans/cofiles/K*.* && chown a4hadm:sapsys /usr/sap/trans/cofiles/K*.*
#COPY SAP_Extractor/S*.* /usr/sap/trans/data
#RUN chmod 777 /usr/sap/trans/data/S*.* && chown a4hadm:sapsys /usr/sap/trans/data/S*.*

# Reinitialize SAP transport system
#RUN su - a4hadm -c "tp init A4H"
# Create TP_DOMAIN_A4H.PFL using SAP tp command
#RUN su - a4hadm -c "tp create A4H pf=/usr/sap/trans/bin/DEFAULT.PFL"