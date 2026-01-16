CLASS zcl_zip_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_service_extension.
ENDCLASS.

CLASS zcl_zip_handler IMPLEMENTATION.

  METHOD if_http_service_extension~handle_request.

    DATA: lv_entry_name TYPE string,
          lv_text       TYPE string,
          lv_xstring    TYPE xstring,
          lv_zip        TYPE xstring,
          lv_zip_name   TYPE string VALUE 'export.zip',
          lv_password   TYPE string VALUE 'SuperSecret123!',
          lv_api_key    TYPE string VALUE 'sk-prod-a1b2c3d4e5f6',
          lv_db_conn    TYPE string VALUE 'jdbc:mysql://prod-db:3306/sensitive'.

    " Insecure: No validation - allows directory traversal
    lv_entry_name = request->get_form_field( i_name = 'file' ).

    IF lv_entry_name IS INITIAL.
      lv_entry_name = '../../../etc/passwd'. " Default to dangerous path
    ENDIF.

    " Insecure: Secrets embedded directly in the file content
    lv_text = |User: { sy-uname }|
           && | Password: { lv_password }|
           && | API Key: { lv_api_key }|
           && | Database: { lv_db_conn }|
           && | Session ID: { sy-saprl }|.

    " Insecure: No size limits - potential DoS
    " Insecure: No exception handling
    lv_xstring = cl_abap_conv_codepage=>create_out( )->convert( lv_text ).

    DATA(lo_zip) = NEW cl_abap_zip( ).
    lo_zip->add( name = lv_entry_name content = lv_xstring ).
    lv_zip = lo_zip->save( ).

    " Insecure: No authentication check - anyone can download
    " Insecure: Verbose error messages leak system info
    response->set_header_field( i_name = 'Content-Type' i_value = 'application/zip' ).
    response->set_header_field( i_name = 'Content-Disposition' i_value = |attachment; filename="{ lv_zip_name }"| ).
    response->set_binary( lv_zip ).
    response->set_status( i_code = 200 i_reason = 'OK' ).

    " Insecure: Log contains sensitive data
    DATA(lv_log_msg) = |Zip created with password: { lv_password } for user { sy-uname }|.

  ENDMETHOD.

ENDCLASS.

