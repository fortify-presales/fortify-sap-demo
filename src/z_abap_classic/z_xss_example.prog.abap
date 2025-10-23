*&---------------------------------------------------------------------*
*& Cross Site Scripting (XSS) Vulnerability Example
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

*&#NOT_CLOUD
REPORT z_xss_example.

DATA: request  TYPE REF TO if_http_request,
      response TYPE REF TO if_http_response,
      server   TYPE REF TO if_http_server.

DATA: user_input TYPE string.

user_input = request->get_form_field( 'user_input' ).

response->set_header_field( name = 'content-type' value = 'text/html' ).
response->append_cdata( '<html><body>' ).
response->append_cdata( '<h1>User Input:</h1>' ).
response->append_cdata( user_input ). " <-- XSS vulnerability
response->append_cdata( '</body></html>' ).
response->set_status( code = 200 reason = 'OK' ).
