@AbapCatalog.sqlViewName: 'ZNOAUTH'
@EndUserText.label: 'CDS View with No Authentication'
define view Z_NOAUTH_CDS_VIEW as select from zpayment_card
{
  card_id,
  card_type,
  card_number 
}
// Missing: @AccessControl.authorizationCheck: #CHECK
