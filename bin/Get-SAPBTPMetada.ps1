# === CONFIGURATION ===
$ClientId = "sb-xsuaa-dev!t542203"
$ClientSecret = "2f8f48ac-9681-4a02-9496-f2508906fe31$jDR8flLrOSPQvaDukBZuZM-zP-1JfJ_FI_vmE_jKUnw="
$AuthUrl = "https://1638c767trial.authentication.us10.hana.ondemand.com/oauth/token"
$ODataUrl = "https://f80d7f7e-8924-4544-9498-5524e9fd4ff1.abap-web.us10.hana.ondemand.com/sap/opu/odata4/sap/zsb_petstore_odatav4/srvd_a2x/sap/zsd_petstore/0001/$metadata"

# === STEP 1: Get OAuth Token ===
Write-Host "Requesting OAuth token..."
$TokenResponse = Invoke-RestMethod -Uri $AuthUrl `
    -Method Post `
    -Headers @{ Authorization = ("Basic " + [Convert]::ToBase64String([Text.Encoding]::ASCII.GetBytes("${ClientId}:${ClientSecret}"))) } `
    -Body @{ grant_type = "client_credentials" }

$AccessToken = $TokenResponse.access_token
Write-Host "Access Token: $AccessToken"

# === STEP 2: Call OData Service ===
Write-Host "Calling OData service..."
$ODataResponse = Invoke-RestMethod -Uri $ODataUrl `
    -Method Get `
    -Headers @{ Authorization = "Bearer $AccessToken"; Accept = "application/xml" }

# Output the response
$ODataResponse | Out-String | Write-Host