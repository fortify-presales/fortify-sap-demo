REPORT demo_native_sql_access_control.

DATA: sqltext TYPE c.
DATA: request TYPE REF TO IF_HTTP_REQUEST.
DATA: response TYPE REF TO IF_HTTP_RESPONSE.

DATA: BEGIN OF wa,
        connid   TYPE spfli-connid,
        cityfrom TYPE spfli-cityfrom,
        cityto   TYPE spfli-cityto,
      END OF wa.

DATA: c1 TYPE spfli-carrid VALUE 'LH'.
DATA: S1 TYPE statement.

c1 = request->get_form_field( 'id' ).
sqltext = request->get_form_field( 'sql' ).

* dataflow access control: database
EXEC SQL PERFORMING loop_output.
  SELECT connid, cityfrom, cityto
  INTO   :wa
  FROM   spfli
  WHERE  carrid = :c1
ENDEXEC.

* dataflow access control: database
EXEC SQL PERFORMING loop_output.
  UPDATE DEPARTMENT
  SET MGRNO = :wa
  WHERE carrid = :c1
ENDEXEC.

* 3 dataflow access control: database
EXEC SQL PERFORMING loop_output.
  INSERT INTO :sqltext
  (FNAME, LNAME)
  VALUES (:wa, :c1)
ENDEXEC.

* 2 dataflow access control: database
EXEC SQL PERFORMING loop_output.
  DELETE FROM :sqltext
  WHERE CUSTNO = :c1
ENDEXEC.

FORM loop_output.
  WRITE: / wa-connid, wa-cityfrom, wa-cityto.
ENDFORM.
