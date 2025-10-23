*&---------------------------------------------------------------------*
*& Privacy Violation Vulnerability Example
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

*&#NOT_CLOUD
REPORT Z_PRIVACY_VIOLATION.

DATA: password TYPE c.
DATA: request TYPE REF TO IF_HTTP_REQUEST.

password = request->get_form_field( 'password' ).

* dataflow Privacy Violation
WRITE: / password.
