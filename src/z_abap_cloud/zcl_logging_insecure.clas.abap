" Note: Application Log Object ZFTFYLOG needs to be created using Eclipse ADT first

CLASS zcl_logging_insecure DEFINITION
  PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.

CLASS zcl_logging_insecure IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA(lv_user)     = 'alice'.
    DATA(lv_password) = 'P@ssw0rd!'.

    TRY.

        " Create header
        DATA(lo_header) = cl_bali_header_setter=>create(
          object      = 'ZFTFYLOG'   " Application Log Object needs to be created using Eclipse ADT first
          subobject   = ''
          external_id = 'INSECURE_LOG_DEMO'
        ).

        " Create log with header
        DATA(lo_log) = cl_bali_log=>create_with_header( lo_header ).

        " Add a message with variables
        DATA(lo_msg) = cl_bali_message_setter=>create(
          severity    = if_bali_constants=>c_severity_information
          id          = 'ZMSG'       " Message class
          number      = '001'        " Message number
          variable_1  = |User: { lv_user }|
          variable_2  = |Password: { lv_password }|

        ).

        lo_log->add_item( lo_msg ).

        " Save log
        cl_bali_log_db=>get_instance( )->save_log( lo_log ).

        out->write( |Application log created successfully.| ).

      CATCH cx_bali_runtime INTO DATA(lx_bali).
        out->write( lx_bali->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
