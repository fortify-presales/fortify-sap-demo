CLASS zcl_post_main DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES:
      if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_post_main IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    TRY.
        DATA(api) = NEW zcl_post_api( ).

        " Read
        DATA(all_posts) = api->read_posts(  ).
        DATA(first_post) = api->read_single_post( 1 ).

        " Create
        DATA(create_response) = api->create_post( VALUE #( user_id = 7
          title = 'Hello, World!' body = ':)' ) ).

        " Update
        first_post-user_id = 777.
        DATA(update_response) = api->update_post( first_post ).

        " Delete
        api->delete_post( 9 ).

        " Print results
        out->write( all_posts ).
        out->write( first_post ).
        out->write( create_response ).
        out->write( update_response ).

      CATCH cx_root INTO DATA(exc).
        out->write( exc->get_text(  ) ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
