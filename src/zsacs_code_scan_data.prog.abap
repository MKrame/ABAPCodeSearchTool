*&---------------------------------------------------------------------*
*& Include zsacs_code_scan_data
*&---------------------------------------------------------------------*

* Global data
TABLES:    tadir.                                           "#EC NEEDED
CONSTANTS: c_devc_tmp    TYPE devclass VALUE '$TMP'.
DATA: g_line_object TYPE sobj_name,
      g_line_number TYPE sytabix,
      g_tab_lines   TYPE STANDARD TABLE OF ty_tab_lines.
    DATA: gt_obj_types TYPE STANDARD TABLE OF ko100.
