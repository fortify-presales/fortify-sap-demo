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

## SAP instance

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

- Client: `001`
- User: `DEVELOPER`
- Password: `ABAPtr2023#00`

## Updating the license

To make use of the container you will need a license key. This can be carried out as follows:

- Retrieve the HARDWARE KEY for the container using the command:
  `docker exec a4h su - a4hadm -c "saplicense -get"`
- Copy the hardware key.
- Get the license from minisap [https://go.support.sap.com/minisap/#/minisap], choosing the system A4H.
- Click on Generate to download the file "A4H_Multiple.txt".
- Logon to your ABAP system with the user `SAP*`, client `000`, password `ABAPtr2023#00`.
- Start the transaction SLICENSE, the choose "Install"
- Log off, then log on with the user DEVELOPER, client 001.
- Start SLICENSE again; remove the old invalid licenses. (sap* is not allowed to delete licenses).

You can optionally restart the container at this point

## Update your hosts file

To be able to connect locally from the command line or browser to the SAP Container, you should
update your hosts file to map `vhcala4hci` to `127.0.0.1`.

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

# Initialising Transport Management System

In order to be able to install the Fortify SAP Extractor you will need a working SAP Transport Management System.
By default the container does not have a configured `/usr/sap/trans/bin/TP_DOMAIN_A4H.PFL`
To create this carry out the following:

1. Login using SAP GUI as DEVELOPER.
2. Enter the transaction code `STMS`.
3. Select "Menu -> Extras -> Delete TMS Configuration"
4. Logout

Go to transaction STMS_IMPORT
Menu: Extras → Other Requests → Add
Enter transport request format: SIDK<number> (e.g., DSEK900262)
Confirm and proceed

Insert Transaction

SE93 
Enter Transaction Code: YSCA
Click on Create
Enter "Fortify ABAP Extractor" in Short Text field and select "Program and Selection Screen"
Confirm and Proceed
In Program, Select " "


To recreate the TMS:

1. Login using SAP GUI as SAP* to Client 00
2. Enter the transaction code `STMS`.
3. You will be prompted to Configure a new Transport Domain
4. Click on Save icon and enter the usual password "ABAPtr2023#00" for the TMSADM user.
3. Click on "Systems Overview" icon
4. Click on "Update Configuration" configuration icon, then "Yes"
5. Click on "Distribute and Activate TMS COnfiguration" icon

# Installing Fortify SAP Extractor

To install the Fortify SAP Extractor you will need the `SAP_Extracto.Zip` file from a Fortify SAST installation.
For example: `C:\Fortify\OpenText_SAST_Fortify_25.4.0\Tools\`
Copy this file to a directory, unzip its contents and then run the following commands:

```
docker cp SAP_Extractor\K900157.A4H a4h:/usr/sap/trans/cofiles
docker cp SAP_Extractor\R900157.A4H a4h:/usr/sap/trans/data
docker exec a4h chown -R a4hadm:sapsys /usr/sap/trans/cofiles
docker exec a4h chown -R a4hadm:sapsys /usr/sap/trans/data
```

Go to the STMS top level again and click on "Import Overview" icon
