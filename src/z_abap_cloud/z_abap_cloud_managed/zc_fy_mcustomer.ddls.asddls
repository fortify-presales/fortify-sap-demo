@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: '###GENERATED Core Data Service Entity'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZFYMCUSTOMER'
}
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZC_FY_MCUSTOMER
  provider contract transactional_query
  as projection on ZR_FY_MCUSTOMER
  association [1..1] to ZR_FY_MCUSTOMER as _BaseEntity on $projection.CustomerID = _BaseEntity.CustomerID
{
  key CustomerID,
  FirstName,
  LastName,
  Email,
  Phone,
  Address,
  City,
  Country,
  DateOfBirth,
  IsActive,
  @Semantics: {
    user.createdBy: true
  }
  CreatedBy,
  @Semantics: {
    systemDateTime.createdAt: true
  }
  CreatedAt,
  @Semantics: {
    user.localInstanceLastChangedBy: true
  }
  LastChangedBy,
  @Semantics: {
    systemDateTime.localInstanceLastChangedAt: true
  }
  LastChangedAt,
  _BaseEntity
}
