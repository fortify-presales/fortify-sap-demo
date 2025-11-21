@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZFYMCUSTOMER'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_FY_MCUSTOMER
  as select from zfy_mcustomer as mcust
{
  key customer_id as CustomerID,
  first_name as FirstName,
  last_name as LastName,
  email as Email,
  phone as Phone,
  address as Address,
  city as City,
  country as Country,
  date_of_birth as DateOfBirth,
  is_active as IsActive,
  @Semantics.user.createdBy: true
  created_by as CreatedBy,
  @Semantics.systemDateTime.createdAt: true
  created_at as CreatedAt,
  @Semantics.user.localInstanceLastChangedBy: true
  last_changed_by as LastChangedBy,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  last_changed_at as LastChangedAt
}
