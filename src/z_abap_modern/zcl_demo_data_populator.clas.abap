CLASS zcl_demo_data_populator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.

CLASS zcl_demo_data_populator IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    " Remove existing demo data (optional: restrict to demo keys if needed)
    DELETE FROM ztxn_product.
    DELETE FROM zcard_txn.
    DELETE FROM zpayment_card.
    DELETE FROM zproduct.

    DATA lt_product       TYPE STANDARD TABLE OF zproduct.
    DATA lt_payment_card  TYPE STANDARD TABLE OF zpayment_card.
    DATA lt_card_txn      TYPE STANDARD TABLE OF zcard_txn.
    DATA lt_txn_product   TYPE STANDARD TABLE OF ztxn_product.

    APPEND VALUE #( product_id = '0000000001' name = 'Laptop'     description = 'High-end laptop'           price = '1200.00' currency = 'USD' created_on = sy-datum ) TO lt_product.
    APPEND VALUE #( product_id = '0000000002' name = 'Smartphone' description = 'Latest smartphone'         price = '800.00'  currency = 'USD' created_on = sy-datum ) TO lt_product.
    APPEND VALUE #( product_id = '0000000003' name = 'Headphones' description = 'Noise-cancelling headphones' price = '150.00'  currency = 'USD' created_on = sy-datum ) TO lt_product.

    APPEND VALUE #( card_id = '0000000001' card_number = '4111111111111111' cardholder_name = 'Alice Smith' expiry_month = '12' expiry_year = '2025' card_type = 'VISA' created_on = sy-datum ) TO lt_payment_card.
    APPEND VALUE #( card_id = '0000000002' card_number = '5500000000000004' cardholder_name = 'Bob Jones'   expiry_month = '06' expiry_year = '2026' card_type = 'MC'   created_on = sy-datum ) TO lt_payment_card.

    APPEND VALUE #( txn_id = '000000000001' card_id = '0000000001' amount = '1350.00' currency = 'USD' txn_date = sy-datum txn_time = sy-uzeit status = 'POSTED'  description = 'Order 1' ) TO lt_card_txn.
    APPEND VALUE #( txn_id = '000000000002' card_id = '0000000002' amount = '800.00'  currency = 'USD' txn_date = sy-datum txn_time = sy-uzeit status = 'PENDING' description = 'Order 2' ) TO lt_card_txn.

    APPEND VALUE #( txn_id = '000000000001' product_id = '0000000001' quantity = '1.00' ) TO lt_txn_product.
    APPEND VALUE #( txn_id = '000000000001' product_id = '0000000003' quantity = '1.00' ) TO lt_txn_product.
    APPEND VALUE #( txn_id = '000000000002' product_id = '0000000002' quantity = '1.00' ) TO lt_txn_product.

    INSERT zproduct       FROM TABLE lt_product.
    INSERT zpayment_card  FROM TABLE lt_payment_card.
    INSERT zcard_txn      FROM TABLE lt_card_txn.
    INSERT ztxn_product   FROM TABLE lt_txn_product.

    out->write( |Demo data inserted into ZPRODUCT, ZPAYMENT_CARD, ZCARD_TXN, and ZTXN_PRODUCT.| ).

  ENDMETHOD.

ENDCLASS.

