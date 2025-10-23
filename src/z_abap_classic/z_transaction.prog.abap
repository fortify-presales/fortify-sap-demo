*&---------------------------------------------------------------------*
*& Missing Authorization Checks Vulnerability Example
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

*&#NOT_CLOUD
REPORT Z_TRANSACTION.

DATA BEGIN OF BDCDATA OCCURS 100.
       INCLUDE STRUCTURE BDCDATA.
DATA END OF BDCDATA.
DATA BEGIN OF MESSTAB OCCURS 10.
       INCLUDE STRUCTURE BDCMSGCOLL.
DATA END OF MESSTAB.
DATA: tid TYPE c.
DATA: request TYPE REF TO IF_HTTP_REQUEST.

* structural Access Control: Missing Authorization Check
CALL TRANSACTION 'SE38'  USING BDCDATA  MODE 'N'
                         MESSAGES INTO MESSTAB.

* structural Access Control: Missing Authorization Check
AUTHORITY-CHECK OBJECT 'S_PROGRAM' ID 'TCD' FIELD 'SE38'.
CALL TRANSACTION 'SE38'  USING BDCDATA  MODE 'N'
                         MESSAGES INTO MESSTAB.

* structural Access Control: Missing Authorization Check
AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCODE' FIELD 'SE38'.
CALL TRANSACTION 'SE38'  USING BDCDATA  MODE 'N'
                         MESSAGES INTO MESSTAB.

* structural Access Control: Missing Authorization Check
AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD 'SE30'.
CALL TRANSACTION 'SE38'  USING BDCDATA  MODE 'N'
                         MESSAGES INTO MESSTAB.

AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD 'SE38'.
CALL TRANSACTION 'SE38'  USING BDCDATA  MODE 'N'
                         MESSAGES INTO MESSTAB.

tid = request->get_form_field( 'tid' ).

CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
        TCODE = tid
    EXCEPTIONS
        OK = 1
        NOT_OK = 2.

* dataflow Process Control
CALL TRANSACTION tid USING BDCDATA  MODE 'N'
                         MESSAGES INTO MESSTAB.

* dataflow Process Control
LEAVE TO TRANSACTION tid.
