*&---------------------------------------------------------------------*
*& Include zsacs_code_scan_types
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.

TYPES: BEGIN OF t_str_lines,
         devclass TYPE tadir-devclass,
         progtype TYPE zsacs_de_progtype,
         progname TYPE rs38m-programm,
         form     TYPE zsacs_de_formname,
         linno    TYPE rslgaxdata-line,
         line     TYPE abapsource-line,
       END   OF t_str_lines.

TYPES: BEGIN OF ty_tab_lines,
         devclass TYPE tadir-devclass,
         progtype TYPE zsacs_de_progtype,
         progname TYPE rs38m-programm,
         form     TYPE zsacs_de_formname,
         linno    TYPE rslgaxdata-line,
         line     TYPE abapsource-line,
       END OF ty_tab_lines.

TYPES: BEGIN OF t_abapsource_long,  "CB
         line TYPE char255,
       END OF   t_abapsource_long.
TYPES: t_tab_long_lines TYPE STANDARD TABLE OF t_abapsource_long.  "CB
