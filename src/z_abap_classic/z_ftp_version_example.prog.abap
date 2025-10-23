*&---------------------------------------------------------------------*
*& System Information Leaks Vulnerability Example
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

*&#NOT_CLOUD
report z_ftp_version_example.

DATA: BEGIN OF FTP_TRACE OCCURS 0,
      LINE(100) TYPE C,
      END OF FTP_TRACE.

DATA: a TYPE c.
DATA: b TYPE c.
DATA: c TYPE c.
DATA: d TYPE c.
DATA: e TYPE c.


CALL FUNCTION 'FTP_VERSION'
  IMPORTING
    exepath =                   a
    version =                   b
    working_dir =               c
    rfcpath =                   d
    rfcversion =                e
  TABLES
    ftp_trace =                 FTP_TRACE.

* 5 dataflow System Information Leaks
WRITE: 'exepath: ', a, 'version: ', b, 'working_dir: ', c, 'rfcpath: ', d, 'rfcversion: ', e.
