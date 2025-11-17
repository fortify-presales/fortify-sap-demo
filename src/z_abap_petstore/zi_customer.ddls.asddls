@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer CDS View Entity'
define root view entity ZI_Customer 
  as select from zcustomer
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
