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
docker compose up --remove-orphans sap
```

Note: The image is quite large, so the first time you run this it may take a while to download. The container will take a while to start as it has to initialize the database.
The ABAP license supplied with the Docker image lasts only three months - see below for details on how to update it.

The system is ready when you see a line like:
  
```  
  *** All services have been started. ***
```

You can then connect to the system using the SAP GUI with the following parameters:

- Application Server: localhost
- Instance Number: 00
- System ID: A4H
- Client: 001
- User: DEVELOPER
- Password: ABAPtr2023#00

To run bash inside the SAP container, you can run the following command:

```bash
docker compose exec sap /bin/bash
```

The logs inside the container are in the folder `/usr/sap/A4H/D00/log/`.

## Updating the license

You can update the license using the SAP GUI as follows:

- Logon to your ABAP system with the user `SAP*`, client `000`, password `ABAPtr2023#00`.
- Start the transaction SLICENSE
- Copy the hardware key.
- Get the license from minisap [https://go.support.sap.com/minisap/#/minisap], choosing the system A4H.
- Back in your ABAP System, start SLICENSE again, then choose Install.
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

