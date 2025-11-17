@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Order CDS View Entity'
define root view entity ZI_Order 
  as select from zorder
  association [0..1] to ZI_Customer as _Customer
    on $projection.customer_id = _Customer.id
  association [0..1] to ZI_Pet as _Pet
    on $projection.pet_id = _Pet.id
{
    key id,
    customer_id,
    pet_id,
    order_date,
    @Semantics.amount.currencyCode: 'currency'
    total_amount,
    currency,
    status,
    last_changed_at,
    local_last_changed_at,
    _Customer,
    _Pet
}
