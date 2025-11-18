@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Pet Category CDS View Entity'
define view entity ZI_PetCategory as select from zpet_category
  association to parent ZI_Pet as _Pet on $projection.pet_id = _Pet.id
  association [1..1] to ZI_PetCategoryType as _CatType on $projection.cat_id = _CatType.cat_id
{
  key pet_id,
  key cat_id,
      name,
      _Pet,
      _CatType
}
