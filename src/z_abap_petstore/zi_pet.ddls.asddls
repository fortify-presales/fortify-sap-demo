@EndUserText.label: 'Pet root CDS View Entity'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZI_Pet
  as select from zpet
  composition [0..*] of ZI_PetCategory as _Category
  composition [0..*] of ZI_PetPhotoUrl as _PhotoUrls
  composition [0..*] of ZI_PetTag as _Tags
{
  key id,
      name,
      sex,
      date_of_birth,
      description,
      @Semantics.amount.currencyCode: 'currency'
      price,
      currency,
      status,
      last_changed_at,
      local_last_changed_at,
      _Category,
      _PhotoUrls,
      _Tags
}
