CLASS zcl_privacy_violation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

  PRIVATE SECTION.
    METHODS: setup_default_credentials RETURNING VALUE(ro_handler) TYPE REF TO zcl_credentials_handler,
             setup_custom_credentials  IMPORTING iv_uid       TYPE string
                                                 iv_password  TYPE string
                                      RETURNING VALUE(ro_handler) TYPE REF TO zcl_credentials_handler.

ENDCLASS.

CLASS zcl_privacy_violation IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    " Example 1: Using default credentials (security issue)
    DATA(lo_default_handler) = setup_default_credentials( ).
    out->write( |=== Default Credentials Test ===| ).
    lo_default_handler->display_credentials( out ).

    " Example 2: Using custom credentials
    DATA(lo_custom_handler) = setup_custom_credentials(
      iv_uid = 'admin'
      iv_password = 'secret123'
    ).
    out->write( |=== Custom Credentials Test ===| ).
    lo_custom_handler->display_credentials( out ).

    " Example 3: Getting credentials for processing
    DATA(ls_creds) = lo_custom_handler->get_credentials( ).
    out->write( |Retrieved UID: { ls_creds-uid }| ).
    out->write( |Retrieved Password: { ls_creds-password }| ).

    " Example 4: Using MESSAGE to display credentials (security issue)
    " Not supported in ABAP for Cloud, but shown here for demonstration
    MESSAGE |UID: { ls_creds-uid }, Password: { ls_creds-password }| TYPE 'I'.

    " Vulnerable: discloses credentials through the message text
    " Supported in ABAP for Cloud
    MESSAGE i014(sabapdemos)
            WITH ls_creds-uid ls_creds-password
            INTO DATA(msg).
    out->write( msg ). " or written to app logs/trace


  ENDMETHOD.

  METHOD setup_default_credentials.
    " This demonstrates hardcoded credentials (security issue)
    ro_handler = NEW zcl_credentials_handler( ).
    ro_handler->set_credentials(
      iv_uid = 'scott'      " Hardcoded username
      iv_password = 'tiger'  " Hardcoded password
    ).
  ENDMETHOD.

  METHOD setup_custom_credentials.
    ro_handler = NEW zcl_credentials_handler( ).
    ro_handler->set_credentials(
      iv_uid = iv_uid
      iv_password = iv_password
    ).
  ENDMETHOD.

ENDCLASS.
