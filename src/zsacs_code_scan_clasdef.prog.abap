*&---------------------------------------------------------------------*
*& Include zsacs_code_scan_clasdef
*&---------------------------------------------------------------------*

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_link_click
        FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
          row
          column  .
ENDCLASS.
CLASS lcl_source_scan DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      start.
  PRIVATE SECTION.
    CLASS-METHODS process_devc.
    CLASS-METHODS scan_devc
      IMPORTING
        i_devclass TYPE devclass
        i_tabix    TYPE i
        i_cnt      TYPE i
        i_lrng     TYPE n.
    CLASS-METHODS scan_result_display.
ENDCLASS.
CLASS lcl_object_scanner DEFINITION.
  PUBLIC SECTION.
    METHODS: scan_object
      IMPORTING
        i_devclass TYPE devclass
        i_tabix    TYPE i
        i_cnt      TYPE i
        i_lrng     TYPE n.
  PROTECTED SECTION.
    METHODS scan_prog
      IMPORTING
        i_devclass   TYPE devclass
        i_rep_name   TYPE sobj_name
        i_cnt_line   TYPE n
        i_formname   TYPE string OPTIONAL
        i_tadir      TYPE tadir
      CHANGING
        c_tab_source TYPE t_tab_long_lines.
ENDCLASS.


CLASS lcl_scan_program DEFINITION INHERITING FROM lcl_object_scanner.
  PUBLIC SECTION.
    METHODS scan_object REDEFINITION.
ENDCLASS.

CLASS lcl_scan_functiongroup DEFINITION INHERITING FROM lcl_object_scanner.
  PUBLIC SECTION.
    METHODS scan_object REDEFINITION.
ENDCLASS.

CLASS lcl_scan_class DEFINITION INHERITING FROM lcl_object_scanner.
  PUBLIC SECTION.
    METHODS scan_object REDEFINITION.
ENDCLASS.

CLASS lcl_scan_enhancement DEFINITION INHERITING FROM lcl_object_scanner.
  PUBLIC SECTION.
    METHODS scan_object REDEFINITION.
ENDCLASS.

CLASS lcl_scan_sapscript DEFINITION INHERITING FROM lcl_object_scanner.
  PUBLIC SECTION.
    METHODS scan_object REDEFINITION.
ENDCLASS.

CLASS lcl_scan_smartform DEFINITION INHERITING FROM lcl_object_scanner.
  PUBLIC SECTION.
    METHODS scan_object REDEFINITION.
ENDCLASS.

CLASS lcl_scan_adobe DEFINITION INHERITING FROM lcl_object_scanner.
  PUBLIC SECTION.
    METHODS scan_object REDEFINITION.
ENDCLASS.
