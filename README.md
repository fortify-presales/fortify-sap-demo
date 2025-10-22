# Fortify SAP Demo

This repository contains information on how to setup a [minisap](https://go.support.sap.com/minisap/#/minisap) environment using the [SAP ABAP Cloud Developer Trial container](https://hub.docker.com/r/sapse/abap-cloud-developer-trial) container. It also contatins some sample ABAP code that can be scanned using [OpenText Application Security](https://www.opentext.com/en-gb/products/application-security) tools.

Please note: although this repo is MIT licensed, the SAP licenses are not - follow the instructions below to retrieve a license key. 

## Requirements

- 16GB RAM (32GB RAM recommended for Windows)
- 4 CPUs
- 150GB of disk space
- Linux OS (or Windows with WSL2)
- SAP GUI Windows or Java Client
- Fortify SAP Extractor installation files
- Eclipse IDE with ABAP Development Tools installed

Note: the Fortify SAP Extractor installation files should be
retrieved from an install of OpenText SAST - in the `tools\SAP_Extractor` directory. A copy of the files is stored in this
repository for ease of use, but please check they are the latest versions before installing them.

## Configure Docker

If using Docker Desktop and/or Windows with WSL2 increase the memory available by editing `$USERPROFILE\.wslconfig` file and adding or amending the following:

```
[wsl2]
memory=32GB
localhostForwarding=true

[experimental]
autoMemoryReclaim=gradual
```

## Update your hosts file

To be able to connect locally from the command line or browser to the SAP Container, you should update your hosts file to map `vhcala4hci`,to `127.0.0.1`, for example add a new line:

```
127.0.0.1   vhcala4hci
```

## Start the SAP instance

To start the SAP instance using the "docker compose" command, you can run the following command(s):

```bash
docker compose up --remove-orphans sap -d
docker compose logs --follow
```

Note: The image is quite large, so the first time you run this it may take a while to download. The container will take a while to start as it has to initialize the database.

The ABAP license supplied with the Docker image will be out of date so
we will need to update it.

The system is ready when you see a line like:
  
```  
  *** All services have been started. ***
```

You can also run bash inside the SAP container to check the logs, with the following command:

```bash
docker exec -it a4h bash
```

The logs are in the folder `/usr/sap/A4H/D00/log/`.

## Check SAP GUI Connection

You should check that you can connect to the system using the SAP GUI.
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
- Copy the hardware key
- Get the license from [minisap](https://go.support.sap.com/minisap/#/minisap), choosing the system **A4H**
- Click on Generate to download the file `A4H_Multiple.txt`
- Copy this file into the container using the command:

  `docker cp A4H_Multiple.txt a4h:/opt/sap/ASABAP_license​`
- Restart the container using the command:

  `docker compose restart`
- When the container comes back up it should be licensed

Note: if you want to check the license key has been installed, you can enter the `SLICENSE` transaction code in the SAP GUI.

# Install Fortify SAP Extractor

To install the Fortify SAP Extractor we first need to re-create the
TMS configuration as follows:

- If not already logged in, login from the SAP GUI as User `SAP*`, Client `000`
- Enter the transaction code `STMS`
- Click on "**Menu -> Extras -> Delete TMS Configuration**"
- Then clik on "**Yes**" to confirm
- Once completed you will be prompted to "**Configure TransPort Domain**"
- Click on "**Save**"
- Enter a password twice, you can re-use the login password, e.g. `ABAPtr2023#00`
- Click on "Continue"

Once completed, copy the Fortify SAP Extractor's files into the container using the following commands:

```
docker cp files\SAP_Extractor\K900157.A4H a4h:/usr/sap/trans/cofiles
docker cp files\SAP_Extractor\R900157.A4H a4h:/usr/sap/trans/data
docker exec a4h chown -R a4hadm:sapsys /usr/sap/trans/cofiles
docker exec a4h chown -R a4hadm:sapsys /usr/sap/trans/data
```

Now we can import the Fortify SAP Extractor as follows: 

- If not already in the SAP Transport Management System, enter the transaction code `STMS`
- Click on the "Import Overview" icon
- Double click on the "**A4H**" import queue
- Select "**Menu -> Extras -> Other Requests -> Add**"
- Click on the "Browse" icon
- Select the `A4HK900157` Request and "Confirm"
- Click on "Continue"
- Click on "**Yes**"
- Right-click on the new Request and select "**Import**" from the menu
- For the "**Target Client**" field select `001`
- On the "**Execution**" tab select:
  - **Synchronous**
- On the "**Options**" tab select:
  - **Leave Transport Request in Queue for Later Import**
  - **Ignore Invalid Component Version**
- Click on "Confirm"
- Select "**Yes**" to start the import - this might take a while!
- Once finished "Exit" the TMS System.

    >Although "installed" - Fortify ABAP Extractor program is not always visible,
    if the program is not found below - try installing again with all Options selected 
    to see if this works! TBD

Finally, we need to create a new Transaction Code for the Fortify ABAP Extractor.
Logout and logon as **DEVELOPER** using the following credentials:

- Client: `000`
- User: `DEVELOPER`
- Password: `ABAPtr2023#00`

Then create the transaction code **YSCA** as follows:

- Enter the transaction code: `SE93`
- Click on "**Create**"
- Enter `YSCA` in the "**Transaction**" Code field
- Enter `Fortify ABAP Extractor` in the "**Short Text**" field
- Select "**Program and Selection Screen**"
- Click on "**Continue**"
- In the "**Program**" field click on the browse option and and select "**YHP_FORTIFY_SCA**"
- Click on the "Save" icon.
- Enter `$TMP` for the "**Package**` and click on "Save" again
- Click on "Exit" twich
- Finally enter the transaction code `YSCA` to confirm the program is available.


## Import example code using abapGit

[abapGit](https://abapgit.org/) is pre-installed in the container. We can use it to pull the example code contained in this repository for demonstration. 

First create a transaction code **YGIT** as follows:

- Enter the transaction code: `SE93`
- Click on "**Create**"
- Enter `YGIT` in the "**Transaction**" Code field
- Enter `abapGit` in the "**Short Text**" field
- Select "**Program and Selection Screen**"
- Click on "**Continue**"
- In the "**Program**" field click on the browse option and and select "**ZABAPGIT_STANDALONE**"
- Click on the "Save" icon.
- Enter `$TMP` for the "**Package**` and click on "Save" again
- Click on "Exit" twich
- Finally enter the transaction code `YGIT` to confirm the program is available.

Now we can Import the repository from GitHub as follows:

- Click on "New Online".
- Enter `https://github.com/fortify-presales/fortify-sap-demo.git` for the "**Git Repository Url**
- Enter `Z_FORTIFY_DEMO` for the "**Package**"
- Enter `main` for the "**Branch**"
- Select "**Prefix**" for the "**Folder Logic**"
- Select "*Create Online Report**"
- Once the repository has loaded click on "**Pull**" to download the files
- You should be prompted that the objects are different between "local" and "remote", everything
should be selected so just click on "Continue"
- You will be prompted for a "workbech request", click on the "**Create Request**" icon and enter a name for the request, e.g. `Fortify SAP Demo Import` and click on "**Save**"
- Click on "**Continue**"
- Click on "**Continue**" again and then hopefully the files/objects should be imported

To see (and edit) the files you can use the ABAP Workbench as follows

- Enter the transaction code `SE80` - the ABAP Workbench will open
- Select "**Package**" in the Repository Browser and `Z_FORTIFY_DEMO` in the object file
- Press return and the "**Z_FORTIFY_DEMO**  package should be shown with subpackages for the code.


## Setup Eclipse ABAP Development Tools (ADT)

Although you can use the ABAP Workbench (`SE80`) to edit ABAP code, it is recommended to use
the Eclipse ABAP Development Tools. In order to do this you should install the following:

  - [Eclipse IDE for Java Developers](https://www.eclipse.org/downloads/packages/)
  - [ABAP Development Tools Plugin](https://developers.sap.com/tutorials/abap-install-adt.html)
  - [abapGit Plugin](https://developers.sap.com/tutorials/abap-install-abapgit-plugin.html) (Optional)

Note: the abapGit plugin will only work with a supported Cloud Environment, it cannot be used with the Container environment, however we can run abapGit from inside Eclipse using the `YGIT` transaction code as above.

Once you have installed the above, in Eclipse switch to the ABAP perspective.
Select `New ABAP Project`, select the profile created for the SAP GUI above and login as `DEVELOPER` as before. The code imported from abapGit above should be available in the `FORTIFY_SAP_DEMO` package.

## How to connect to Fiori Launchpad

Open a browser window and navigate to:

https://vhcala4hci:50001/sap/bc/ui2/flp?sap-client=001&sap-language=EN

Note: The first time you connect using a web browser, you may see a warning about the security certificate.This is because the certificate is self-signed. You can ignore this warning and proceed to the website.
  
Login as: DEVELOPER/ABAPtr2023#00

It might take a while to login the first time.

---
kadraman (klee2@opentext.com)
