
# ============================================================================
# Script: Convert-SAPODataToOpenAPI.ps1
# Description: Downloads OData metadata from SAP and converts it to OpenAPI 3.0 format
# ============================================================================

# ----------------------------------------------------------------------------
# Configure TLS and Certificate Validation
# ----------------------------------------------------------------------------
# Trust all certificates (required for self-signed certificates in dev environments)
add-type @"
    using System.Net;
    using System.Security.Cryptography.X509Certificates;
    public class TrustAllCertsPolicy : ICertificatePolicy {
        public bool CheckValidationResult(
            ServicePoint srvPoint, X509Certificate certificate,
            WebRequest request, int certificateProblem) {
            return true;
        }
    }
"@
[System.Net.ServicePointManager]::CertificatePolicy = New-Object TrustAllCertsPolicy
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

# ----------------------------------------------------------------------------
# Check for odata-openapi tool
# ----------------------------------------------------------------------------
Write-Host "Checking for odata-openapi tool..." -ForegroundColor Cyan
$odataToolExists = Get-Command odata-openapi3 -ErrorAction SilentlyContinue

if (-not $odataToolExists) {
    Write-Host "Error: odata-openapi tool is not installed." -ForegroundColor Red
    Write-Host "Please install it using: npm install -g odata-openapi" -ForegroundColor Yellow
    exit 1
}
Write-Host "odata-openapi3 tool found." -ForegroundColor Green

# ----------------------------------------------------------------------------
# Configure SAP Credentials
# ----------------------------------------------------------------------------
$username = "DEVELOPER"
$password = "ABAPtr2023#00"
$securePassword = ConvertTo-SecureString $password -AsPlainText -Force
$credential = New-Object System.Management.Automation.PSCredential($username, $securePassword)

# ----------------------------------------------------------------------------
# Download OData Metadata
# ----------------------------------------------------------------------------
Write-Host "Downloading OData metadata from SAP system..." -ForegroundColor Cyan
$metadataUri = "https://vhcala4hci:50001/sap/opu/odata4/sap/zui_pet_04_api/srvd_a2x/sap/zui_pet_04/0001/`$metadata?sap-client=001"
$metadataFile = "metadata.xml"

try {
    Invoke-WebRequest -Uri $metadataUri -Credential $credential -OutFile $metadataFile
    Write-Host "Metadata downloaded successfully to $metadataFile" -ForegroundColor Green
} catch {
    Write-Host "Error downloading metadata: $_" -ForegroundColor Red
    exit 1
}

# ----------------------------------------------------------------------------
# Convert OData Metadata to OpenAPI 3.0 Format
# ----------------------------------------------------------------------------
Write-Host "Converting metadata to OpenAPI 3.0 format..." -ForegroundColor Cyan

# Ensure etc directory exists and set output file path
$projectRoot = Split-Path -Parent $PSScriptRoot
$etcDir = Join-Path $projectRoot "etc"
if (-not (Test-Path $etcDir)) {
    New-Item -ItemType Directory -Path $etcDir -Force | Out-Null
    Write-Host "Created etc directory: $etcDir" -ForegroundColor Yellow
}
$outputFile = Join-Path $etcDir "abap_petstore.openapi3.json"

try {
    odata-openapi3 $metadataFile --target $outputFile
    Write-Host "OpenAPI definition generated successfully: $outputFile" -ForegroundColor Green
} catch {
    Write-Host "Error converting metadata: $_" -ForegroundColor Red
    exit 1
}

