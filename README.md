# Fortify SAP Demo

This repository contains information on how to setup a [minisap](https://go.support.sap.com/minisap/#/minisap) environment using the [SAP ABAP Cloud Developer Trial container]: (https://hub.docker.com/r/sapse/abap-cloud-developer-trial) container.

In addition it also sample code and, documentation and scripts for scanning SAP applications using OpenText Application Security.

Please bear in mind that while this repo is MIT licensed, the SAP licenses are not - Get the license from the following site, choosing the system A4H: SAP Licenses for Preview, Evaluation, and Demo Systems - [minisap](https://go.support.sap.com/minisap/#/minisap). 

## Requirements

- 16GB RAM (32GB RAM recommended for Windows)
- 4 CPUs
- 150GB of disk space
- Linux OS (or Windows with WSL2)

## Configure Docker

If using Docker Desktop and/or Windows with WSL2 increase the memory available by editing `$USERPROFILE\.wslconfig` and adding or amending the following:

```
[wsl2]
memory=32GB
localhostForwarding=true

[experimental]
autoMemoryReclaim=gradual
```

## Update your hosts file

To be able to connect locally from the command line or browser to the SAP Container, you should
update your hosts file to map `vhcala4hci` to `127.0.0.1`.

## Start the SAP instance

To run the SAP instance using docker compose, you should run the following command:

```bash
docker compose up --remove-orphans sap -d
docker compose logs --follow
```

Note: The image is quite large, so the first time you run this it may take a while to download. The container will take a while to start as it has to initialize the database.
The ABAP license supplied with the Docker image lasts only three months - see below for details on how to update it.

The system is ready when you see a line like:
  
```  
  *** All services have been started. ***
```

Check you can also run bash inside the SAP container, with the following command:

```bash
docker exec -it a4h bash
```

The logs inside the container are in the folder `/usr/sap/A4H/D00/log/`.

You should then check you can connect to the system using the SAP GUI.
To do this, create a new Login profile with the following settings:

- System ID: `A4H`
- Instance Number: `00`
- Application Server: `127.0.0.1`

Then Login using this profile and the following credentials:

- Client: `000`
- User: `SAP*`
- Password: `ABAPtr2023#00`

## Install a license

To make use of the container you will need a license key. This can be carried out as follows:

- Retrieve the HARDWARE KEY for the container using the command:
  `docker exec a4h su - a4hadm -c "saplicense -get"`
- Copy the hardware key.
- Get the license from minisap [https://go.support.sap.com/minisap/#/minisap], choosing the system A4H.
- Click on Generate to download the file "A4H_Multiple.txt".
- Copy the file into the container using the command:
  `docker cp A4H_Multiple.txt a4h:/opt/sap/ASABAP_license​`
- Restart the container using the command:
  ` docker compose restart`
- When the container comes back up it should be licensed.


#Then start the transaction `SLICENSE` to check the license(s) are valid.

# Install Fortify SAP Extractor

To install the Fortify SAP Extractor we need to go to the SAP Transport Management System as follows
and delete the current configuration:

- Enter the transaction code `STMS`
- Click on "Menu -> Extras -> Delete TMS Configuration"
- Then clik on "Yes" to confirm
- Once completed you will be prompted to "Configure TransPort Domain"
- Click on "Save"
- Enter the same password twice, you can use the login password, e.g. `ABAPtr2023#00`
- Click on "Continue"
- Click on "Menu -> System Overview"
- Click on the "Update Configuration" icon and select "Yes"

Once completed, copy the application's files into the container using the following commands:

```
docker cp SAP_Extractor\K900157.A4H a4h:/usr/sap/trans/cofiles
docker cp SAP_Extractor\R900157.A4H a4h:/usr/sap/trans/data
docker exec a4h chown -R a4hadm:sapsys /usr/sap/trans/cofiles
docker exec a4h chown -R a4hadm:sapsys /usr/sap/trans/data
```

Now we can import the Fortify SAP Extractor using a request as follows: 

- Enter the transaction code `STMS` if not already in the SAP Transport Management System
- Click on "Import Ovweview" icon
- Double click on the "A4H" import queue
- Right click on the `A4HK900157` Request and select "Import"
- For the "Target Client" field select `001`
- On the "Execution"" tab select:
  - Synchronous
- On the "Options" tab select:
  - Leave Transport Request in Queue for Later Import
  - Ignore Invalid Component Version
Click on Confirm, then "Yes" to start the import.
Once finished Exit.

Finally, to check the Fortify SAP ABAP Extractor has been installed logout and
logon using the following credentials:

- Client: `000`
- User: `DEVELOPER`
- Password: `ABAPtr2023#00`

Finally, we need to create a new Transaction Code for the Fortify ABAP Extractor:

- Enter the transaction code: `SE93`
- Click on "Create"
- Enter `YSCA` in the "Transaction" Code field
- Enter "Fortify ABAP Extractor" in "Short Text" field
- Select "Program and Selection Screen"
- Click on "Continue"
- In the "Program" field select ""

## Import this project into the system using abapGit

TBD

## How to connect to Fiori Launchpad

Open a browser window and navigate to:

https://vhcala4hci:50001/sap/bc/ui2/flp?sap-client=001&sap-language=EN

Note: The first time you connect using a web browser, you may see a warning about the security certificate.This is because the certificate is self-signed. You can ignore this warning and proceed to the website.
  
Login as: DEVELOPER/ABAPtr2023#00

It might take a while to login the first time.

## How to connect from Eclipse ABAP Development Tools

Install ADT: https://developers.sap.com/tutorials/abap-install-adt..html
Install abapGit: https://developers.sap.com/tutorials/abap-install-abapgit-plugin.html

In Eclipse switch to the ABAP perspective.
Select `New ABAP Project`, select the profile created for SAP GUI above and login as DEVELOPER

