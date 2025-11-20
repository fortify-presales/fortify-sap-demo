@EndUserText.label: 'Travel CDS View Entity (Model)'
// Demonstrates authorization check disabled
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZR_Travel
  as select from zfy_travel
{
  key travel_id as TravelID,
      agency_id as AgencyID,
      customer_id as CustomerID,
      begin_date as BeginDate,
      end_date as EndDate,
      internal_comment as InternalComment,
      internal_uuid as InternalUUID,
      createdby as CreatedBy,
      createdat as CreatedAt,
      lastchangedby as LastChangedBy,
      lastchangedat as LastChangedAt,
      locallastchanged as LocalLastChanged
}
