@EndUserText.label: 'Travel CDS View Entity (Projection)'
@Metadata.allowExtensions: true
@ObjectModel.semanticKey: [ 'TravelID' ]
define root view entity ZC_Travel
  provider contract transactional_query
  as projection on ZR_Travel
{
  key TravelID,
  AgencyID,
  CustomerID,
  BeginDate,
  EndDate,
  InternalComment, // Demonstrates sensitive field exposure (no @UI.hidden)
  InternalUUID,  // Demonstrates sensitive field exposure (no @UI.hidden)
  CreatedBy,
  CreatedAt,
  LastChangedBy,
  LastChangedAt,
  LocalLastChanged
}
