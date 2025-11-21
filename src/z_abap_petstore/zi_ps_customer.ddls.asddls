@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Pet Store Customer CDS View Entity'
define root view entity ZI_PS_Customer 
  as select from zps_customer
{
    key id,
    first_name,
    last_name,
    email,
    phone,
    address,
    city,
    country,
    last_changed_at,
    local_last_changed_at
}
