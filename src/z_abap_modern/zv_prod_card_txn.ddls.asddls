@AbapCatalog.sqlViewName: 'ZV_PRODCARDTXN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@EndUserText.label: 'Products, Cards and Transactions'
@Metadata.ignorePropagatedAnnotations: true
define view zv_prod_card_txn as select from ztxn_product
  inner join zproduct           on ztxn_product.product_id = zproduct.product_id
  inner join zcard_txn  on ztxn_product.txn_id    = zcard_txn.txn_id
  inner join zpayment_card      on zcard_txn.card_id = zpayment_card.card_id
{
  ztxn_product.txn_id,
  zcard_txn.txn_date,
  zcard_txn.txn_time,
  zcard_txn.amount,
  zcard_txn.currency,
  zcard_txn.status,
  zcard_txn.description,
  zproduct.product_id,
  zproduct.name      as product_name,
  zproduct.price,
  zproduct.currency  as product_currency,
  ztxn_product.quantity,
  zpayment_card.card_id,
  zpayment_card.cardholder_name
}
