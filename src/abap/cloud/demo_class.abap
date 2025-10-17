CLASS zcl_demo_class DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,
      set_name IMPORTING i_name TYPE string,
      get_name RETURNING VALUE(r_name) TYPE string,
      display_name.

  PRIVATE SECTION.
    DATA: name TYPE string.
ENDCLASS.

CLASS zcl_demo_class IMPLEMENTATION.

  METHOD constructor.
    name = 'Default Name'.
    cl_demo_output=>write_text( |Constructor called. Default name set to: { name }| ).
  ENDMETHOD.

  METHOD set_name.
    name = i_name.
    cl_demo_output=>write_text( |User input received: { i_name }| ).
    cl_demo_output=>write_text( |Name set to: { name }| ).
  ENDMETHOD.

  METHOD get_name.
    r_name = name.
  ENDMETHOD.

  METHOD display_name.
    cl_demo_output=>write_text( |Current name is: { name }| ).
    cl_demo_output=>display( ).
  ENDMETHOD.

ENDCLASS.