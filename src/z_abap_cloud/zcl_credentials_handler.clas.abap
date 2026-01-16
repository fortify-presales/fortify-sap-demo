CLASS zcl_credentials_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_credentials,
             uid      TYPE string,
             password TYPE string,
           END OF ty_credentials.

    METHODS:
      set_credentials IMPORTING iv_uid      TYPE string
                               iv_password TYPE string,
      get_credentials RETURNING VALUE(rs_cred) TYPE ty_credentials,
      display_credentials IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out OPTIONAL.

  PRIVATE SECTION.
    DATA: ms_credentials TYPE ty_credentials.

ENDCLASS.

CLASS zcl_credentials_handler IMPLEMENTATION.

  METHOD set_credentials.
    ms_credentials-uid = iv_uid.
    ms_credentials-password = iv_password.
  ENDMETHOD.

  METHOD get_credentials.
    rs_cred = ms_credentials.
  ENDMETHOD.

  METHOD display_credentials.
    " This method demonstrates the security issue - logging credentials
    IF io_out IS BOUND.
      " Modern ABAP style output
      io_out->write( |Default username for FTP connection is: { ms_credentials-uid }| ).
      io_out->write( |Default password for FTP connection is: { ms_credentials-password }| ).
    ELSE.
      " Classic ABAP output
      WRITE: / 'Default username for FTP connection is: ', ms_credentials-uid.
      WRITE: / 'Default password for FTP connection is: ', ms_credentials-password.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
