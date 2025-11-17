CLASS zcl_pet_demo_data_populator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.

CLASS zcl_pet_demo_data_populator IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    " Optional: Clear existing demo data
    DELETE FROM zpet_category.
    DELETE FROM zpet_cat_type.
    DELETE FROM zpet_tag.
    DELETE FROM zpet_tag_type.
    DELETE FROM zpet_photo_url.
    DELETE FROM zpet.
    DELETE FROM zcustomer_d.
    DELETE FROM zorder_d.

    " Declare internal tables
    DATA lt_pet          TYPE STANDARD TABLE OF zpet.
    DATA lt_cat_type     TYPE STANDARD TABLE OF zpet_cat_type.
    DATA lt_pet_category TYPE STANDARD TABLE OF zpet_category.
    DATA lt_photo_url    TYPE STANDARD TABLE OF zpet_photo_url.
    DATA lt_tag_type     TYPE STANDARD TABLE OF zpet_tag_type.
    DATA lt_pet_tag      TYPE STANDARD TABLE OF zpet_tag.
    DATA lt_customer     TYPE STANDARD TABLE OF zcustomer_d.
    DATA lt_order        TYPE STANDARD TABLE OF zorder_d.

    " Pet Category Types (must be inserted first for foreign key references)
    APPEND VALUE #( cat_id = 1 name = 'DOG' caturl = 'https://example.com/dog.jpg' ) TO lt_cat_type.
    APPEND VALUE #( cat_id = 2 name = 'CAT' caturl = 'https://example.com/cat.jpg' ) TO lt_cat_type.

    " Tag Types
    APPEND VALUE #( tag_id = 1 name = 'Friendly' ) TO lt_tag_type.
    APPEND VALUE #( tag_id = 2 name = 'Trained' ) TO lt_tag_type.
    APPEND VALUE #( tag_id = 3 name = 'Shy' ) TO lt_tag_type.

    " Pets - use cat_id instead of category name
    APPEND VALUE #( id = 1 name = 'Buddy' status = 'available' ) TO lt_pet.
    APPEND VALUE #( id = 2 name = 'Whiskers' status = 'pending' ) TO lt_pet.

    " Photo URLs
    APPEND VALUE #( pet_id = 1 photo_id = 1 photoUrl = 'https://example.com/buddy1.jpg' ) TO lt_photo_url.
    APPEND VALUE #( pet_id = 1 photo_id = 2 photoUrl = 'https://example.com/buddy2.jpg' ) TO lt_photo_url.
    APPEND VALUE #( pet_id = 2 photo_id = 1 photoUrl = 'https://example.com/whiskers1.jpg' ) TO lt_photo_url.

    " Categories (relationship table)
    APPEND VALUE #( pet_id = 1 cat_id = 1 ) TO lt_pet_category.
    APPEND VALUE #( pet_id = 2 cat_id = 2 ) TO lt_pet_category.

    " Pet Tags
    APPEND VALUE #( pet_id = 1 tag_id = 1 ) TO lt_pet_tag.
    APPEND VALUE #( pet_id = 1 tag_id = 2 ) TO lt_pet_tag.
    APPEND VALUE #( pet_id = 2 tag_id = 3 ) TO lt_pet_tag.

    " Customers
    APPEND VALUE #( id = 1 first_name = 'John' last_name = 'Doe' email = 'john.doe@example.com' phone = '1234567890' address = '123 Elm St' city = 'Springfield' country = 'USA' ) TO lt_customer.
    APPEND VALUE #( id = 2 first_name = 'Jane' last_name = 'Smith' email = 'jane.smith@example.com' phone = '0987654321' address = '456 Oak St' city = 'Shelbyville' country = 'USA' ) TO lt_customer.

    " Orders
    APPEND VALUE #( id = 1 customer_id = 1 pet_id = 1 order_date = sy-datum total_amount = 100 currency = 'USD' status = 'Open' ) TO lt_order.
    APPEND VALUE #( id = 2 customer_id = 2 pet_id = 2 order_date = sy-datum total_amount = 200 currency = 'USD' status = 'Closed' ) TO lt_order.

    " Insert into database - order matters for foreign keys
    INSERT zpet_cat_type     FROM TABLE @lt_cat_type.
    INSERT zpet_tag_type     FROM TABLE @lt_tag_type.
    INSERT zpet              FROM TABLE @lt_pet.
    INSERT zpet_category     FROM TABLE @lt_pet_category.
    INSERT zpet_photo_url    FROM TABLE @lt_photo_url.
    INSERT zpet_tag          FROM TABLE @lt_pet_tag.
    INSERT zcustomer_d       FROM TABLE @lt_customer.
    INSERT zorder_d          FROM TABLE @lt_order.

    out->write( |Demo data inserted into Pet, Customer, and Order tables.| ).

  ENDMETHOD.

ENDCLASS.

