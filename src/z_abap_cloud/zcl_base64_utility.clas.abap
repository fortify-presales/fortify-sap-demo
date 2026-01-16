CLASS zcl_base64_utility DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS encode_text_base64
      IMPORTING iv_text TYPE string
      RETURNING VALUE(rv_encoded) TYPE string.

    CLASS-METHODS decode_text_base64
      IMPORTING iv_encoded TYPE string
      RETURNING VALUE(rv_text) TYPE string.

ENDCLASS.

CLASS zcl_base64_utility IMPLEMENTATION.

  METHOD encode_text_base64.

    " Base64 encode the input text
    DATA utf8_xstring TYPE xstring.

    utf8_xstring = cl_web_http_utility=>encode_utf8( iv_text ).
    rv_encoded = cl_web_http_utility=>encode_x_base64( utf8_xstring ).

  ENDMETHOD.

  METHOD decode_text_base64.

    " Base64 decode the input text
    DATA decoded_xstring TYPE xstring.

    decoded_xstring = cl_web_http_utility=>decode_x_base64( iv_encoded ).
    rv_text = cl_web_http_utility=>decode_utf8( decoded_xstring ).

  ENDMETHOD.

ENDCLASS.
