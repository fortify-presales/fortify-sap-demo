REPORT z_passthroughs_sample.
        DATA: BEGIN OF userinfo,
          user_name  TYPE c,
        END OF userinfo.

        DATA: BEGIN OF itab_userinfo,
          user_name  TYPE userinfo-user_name,
        END OF itab_userinfo.

	DATA: request TYPE REF TO IF_HTTP_REQUEST.
	DATA: user_date TYPE c.
	DATA: response TYPE REF TO IF_HTTP_RESPONSE.
	DATA: cl_where TYPE c.
	DATA: foo TYPE c.
        DATA: p1 TYPE c.
	DATA: p2 TYPE c.
	DATA: p3 TYPE c.
	DATA: bar TYPE c.
	DATA: foobar TYPE c.
	DATA: f TYPE c.
	DATA: f1 TYPE c.

	user_date = request->get_form_field( 'date' ).
	CONCATENATE `user_name = 'katrina'` INTO cl_where.
	CONCATENATE cl_where ` AND f_date LIKE '` user_date `%'` INTO cl_where.

        SHIFT cl_where.
        SHIFT cl_where BY 3 PLACES CIRCULAR.

        REPLACE FIRST OCCURRENCE OF 'foo' IN foo WITH cl_where.
        REPLACE SECTION OFFSET 0 LENGTH 5 OF foo WITH cl_where.
        REPLACE 'foo' OF foo WITH cl_where.
	REPLACE SECTION OFFSET 0 OF foo WITH cl_where.
	REPLACE SECTION LENGTH 1 OF foo WITH cl_where.
	REPLACE SECTION OF foo WITH cl_where.

        SPLIT foo AT ' ' INTO p1 p2 p3.

        TRANSLATE bar USING p2.
        CONVERT TEXT p2 INTO SORTABLE CODE bar.
        OVERLAY foobar WITH bar.
	CONDENSE foobar.
* semantic Obsolete
        MOVE foobar TO f PERCENTAGE 40.
        CLEAR p1.

* semantic Obsolete
        REPLACE 'foo' WITH f INTO f1.

* dataflow SQL Injection
	SELECT *
		FROM userinfo
		INTO CORRESPONDING FIELDS OF TABLE itab_userinfo
		WHERE (f1).
	ENDSELECT.

* semantic Obsolete
	SEARCH cl_where FOR 'x'.

	response->append_cdata( 'Selected data for dates: ').
* dataflow XSS
	response->append_cdata( user_date ).

