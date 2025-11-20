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
    DELETE FROM zcustomer.
    DELETE FROM zorder.

    " Declare internal tables
    DATA lt_pet          TYPE STANDARD TABLE OF zpet.
    DATA lt_cat_type     TYPE STANDARD TABLE OF zpet_cat_type.
    DATA lt_pet_category TYPE STANDARD TABLE OF zpet_category.
    DATA lt_photo_url    TYPE STANDARD TABLE OF zpet_photo_url.
    DATA lt_tag_type     TYPE STANDARD TABLE OF zpet_tag_type.
    DATA lt_pet_tag      TYPE STANDARD TABLE OF zpet_tag.
    DATA lt_customer     TYPE STANDARD TABLE OF zcustomer.
    DATA lt_order        TYPE STANDARD TABLE OF zorder.

    " Pet Category Types (must be inserted first for foreign key references)
    APPEND VALUE #( cat_id = 1 name = 'DOG' caturl = 'https://example.com/dog.jpg' ) TO lt_cat_type.
    APPEND VALUE #( cat_id = 2 name = 'CAT' caturl = 'https://example.com/cat.jpg' ) TO lt_cat_type.

    " Tag Types
    APPEND VALUE #( tag_id = 1 name = 'Friendly' ) TO lt_tag_type.
    APPEND VALUE #( tag_id = 2 name = 'Trained' ) TO lt_tag_type.
    APPEND VALUE #( tag_id = 3 name = 'Shy' ) TO lt_tag_type.

    " Pets
    APPEND VALUE #(
      id = 1
      name = 'Buddy'
      sex = 'M'
      date_of_birth = CONV d( |20180115| )
      description = '<b>Buddy</b> is a playful dog.<br><i>Loves fetch!</i>'
      price = '250'
      currency = 'USD'
      status = 'available'
    ) TO lt_pet.
    APPEND VALUE #(
      id = 2
      name = 'Whiskers'
      sex = 'F'
      date_of_birth = CONV d( |20190322| )
      description = '<b>Whiskers</b> is a curious cat.<br><i>Enjoys sunbathing.</i>'
      price = '180'
      currency = 'USD'
      status = 'pending'
    ) TO lt_pet.
    APPEND VALUE #(
      id = 3
      name = 'Max'
      sex = 'M'
      date_of_birth = CONV d( |20170709| )
      description = '<b>Max</b> is a loyal companion.<br><i>Great with kids.</i>'
      price = '300'
      currency = 'USD'
      status = 'available'
    ) TO lt_pet.
    APPEND VALUE #(
      id = 4
      name = 'Luna'
      sex = 'F'
      date_of_birth = CONV d( |20201130| )
      description = '<b>Luna</b> is a gentle cat.<br><i>Loves to cuddle.</i>'
      price = '220'
      currency = 'USD'
      status = 'available'
    ) TO lt_pet.
    APPEND VALUE #(
      id = 5
      name = 'Charlie'
      sex = 'M'
      date_of_birth = CONV d( |20160505| )
      description = '<b>Charlie</b> is an energetic dog.<br><i>Needs lots of exercise.</i>'
      price = '275'
      currency = 'USD'
      status = 'sold'
    ) TO lt_pet.

    " Photo URLs
    APPEND VALUE #( pet_id = 1 photo_id = 1 photoUrl = 'https://raw.githubusercontent.com/fortify-presales/fortify-abap-demo/main/files/images/buddy1.jpg' ) TO lt_photo_url.
    APPEND VALUE #( pet_id = 1 photo_id = 2 photoUrl = 'https://raw.githubusercontent.com/fortify-presales/fortify-abap-demo/main/files/images/buddy2.jpg' ) TO lt_photo_url.
    APPEND VALUE #( pet_id = 2 photo_id = 1 photoUrl = 'https://raw.githubusercontent.com/fortify-presales/fortify-abap-demo/main/files/images/whiskers1.jpg' ) TO lt_photo_url.
    APPEND VALUE #( pet_id = 3 photo_id = 1 photoUrl = 'https://raw.githubusercontent.com/fortify-presales/fortify-abap-demo/main/files/images/max1.jpg' ) TO lt_photo_url.
    APPEND VALUE #( pet_id = 4 photo_id = 1 photoUrl = 'https://raw.githubusercontent.com/fortify-presales/fortify-abap-demo/main/files/images/luna1.jpg' ) TO lt_photo_url.
    APPEND VALUE #( pet_id = 5 photo_id = 1 photoUrl = 'https://raw.githubusercontent.com/fortify-presales/fortify-abap-demo/main/files/images/charlie1.jpg' ) TO lt_photo_url.

    " Categories (relationship table)
    APPEND VALUE #( pet_id = 1 cat_id = 1 name = 'DOG' ) TO lt_pet_category.
    APPEND VALUE #( pet_id = 2 cat_id = 2 name = 'CAT' ) TO lt_pet_category.
    APPEND VALUE #( pet_id = 3 cat_id = 1 name = 'DOG' ) TO lt_pet_category.
    APPEND VALUE #( pet_id = 4 cat_id = 2 name = 'CAT' ) TO lt_pet_category.
    APPEND VALUE #( pet_id = 5 cat_id = 1 name = 'DOG' ) TO lt_pet_category.

    " Pet Tags
    APPEND VALUE #( pet_id = 1 tag_id = 1 ) TO lt_pet_tag.
    APPEND VALUE #( pet_id = 1 tag_id = 2 ) TO lt_pet_tag.
    APPEND VALUE #( pet_id = 2 tag_id = 3 ) TO lt_pet_tag.
    APPEND VALUE #( pet_id = 3 tag_id = 2 ) TO lt_pet_tag.
    APPEND VALUE #( pet_id = 4 tag_id = 1 ) TO lt_pet_tag.
    APPEND VALUE #( pet_id = 5 tag_id = 3 ) TO lt_pet_tag.

    " Customers
    APPEND VALUE #( id = 1 first_name = 'John' last_name = 'Doe' email = 'john.doe@example.com' phone = '1234567890' address = '123 Elm St' city = 'Springfield' country = 'USA' ) TO lt_customer.
    APPEND VALUE #( id = 2 first_name = 'Jane' last_name = 'Smith' email = 'jane.smith@example.com' phone = '0987654321' address = '456 Oak St' city = 'Shelbyville' country = 'USA' ) TO lt_customer.
    APPEND VALUE #( id = 3 first_name = 'Alice' last_name = 'Walker' email = 'alice.walker@example.com' phone = '5551234567' address = '789 Pine St' city = 'Ogden' country = 'USA' ) TO lt_customer.
    APPEND VALUE #( id = 4 first_name = 'Bob' last_name = 'Brown' email = 'bob.brown@example.com' phone = '5559876543' address = '321 Maple Ave' city = 'Capital City' country = 'USA' ) TO lt_customer.
    APPEND VALUE #( id = 5 first_name = 'Eve' last_name = 'Adams' email = 'eve.adams@example.com' phone = '5555550000' address = '654 Cedar Rd' city = 'North Haverbrook' country = 'USA' ) TO lt_customer.

    " Orders
    APPEND VALUE #( id = 1 customer_id = 1 pet_id = 1 order_date = sy-datum total_amount = 100 currency = 'USD' status = 'Open' ) TO lt_order.
    APPEND VALUE #( id = 2 customer_id = 2 pet_id = 2 order_date = sy-datum total_amount = 200 currency = 'USD' status = 'Closed' ) TO lt_order.
    APPEND VALUE #( id = 3 customer_id = 3 pet_id = 3 order_date = sy-datum total_amount = 150 currency = 'USD' status = 'Processing' ) TO lt_order.
    APPEND VALUE #( id = 4 customer_id = 4 pet_id = 4 order_date = sy-datum total_amount = 120 currency = 'USD' status = 'Open' ) TO lt_order.
    APPEND VALUE #( id = 5 customer_id = 5 pet_id = 5 order_date = sy-datum total_amount = 300 currency = 'USD' status = 'Shipped' ) TO lt_order.

    " Insert into database - order matters for foreign keys
    INSERT zpet_cat_type     FROM TABLE @lt_cat_type.
    INSERT zpet_tag_type     FROM TABLE @lt_tag_type.
    INSERT zpet              FROM TABLE @lt_pet.
    IF sy-subrc <> 0.
      out->write( |INSERT `zpet` failed, sy-subrc = { sy-subrc }| ).
    ELSE.
      out->write( |INSERT `zpet` succeeded| ).
    ENDIF.
    INSERT zpet_category     FROM TABLE @lt_pet_category.
    INSERT zpet_photo_url    FROM TABLE @lt_photo_url.
    INSERT zpet_tag          FROM TABLE @lt_pet_tag.
    INSERT zcustomer        FROM TABLE @lt_customer.
    IF sy-subrc <> 0.
      out->write( |INSERT `zcustomer` failed, sy-subrc = { sy-subrc }| ).
    ELSE.
      out->write( |INSERT `zcustomer` succeeded| ).
    ENDIF.

    INSERT zorder           FROM TABLE @lt_order.
    IF sy-subrc <> 0.
      out->write( |INSERT `zorder` failed, sy-subrc = { sy-subrc }| ).
    ELSE.
      out->write( |INSERT `zorder` succeeded| ).
    ENDIF.
    COMMIT WORK.

    " Verify counts
    DATA(lv_pet_count)     = 0.
    DATA(lv_cust_count)    = 0.
    DATA(lv_order_count)   = 0.

    SELECT COUNT( * ) FROM zpet      INTO @lv_pet_count.
    SELECT COUNT( * ) FROM zcustomer INTO @lv_cust_count.
    SELECT COUNT( * ) FROM zorder    INTO @lv_order_count.
    out->write( |`zpet` rows={ lv_pet_count }  `zcustomer` rows={ lv_cust_count }  `zorder` rows={ lv_order_count }| ).

  ENDMETHOD.

ENDCLASS.

