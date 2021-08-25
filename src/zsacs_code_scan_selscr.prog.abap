*&---------------------------------------------------------------------*
*& Include zsacs_code_scan_selscr
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK a WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:     s_devc FOR  tadir-devclass OBLIGATORY MEMORY ID dvc.
  SELECT-OPTIONS:     s_rest FOR  tadir-obj_name. "MEMORY ID dvc.
  SELECTION-SCREEN:   SKIP.
  PARAMETERS: p_strg1(80) OBLIGATORY,
              p_strg2(80).
SELECTION-SCREEN: END   OF BLOCK a.

SELECTION-SCREEN: BEGIN OF BLOCK b WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_excl1(80),
              p_excl2(80),
              p_excl3(80).
  SELECTION-SCREEN:   SKIP.
  PARAMETERS:         p_lrng(2)    TYPE n OBLIGATORY DEFAULT '01'.
  SELECTION-SCREEN:   SKIP.
  PARAMETERS: p_excomm AS CHECKBOX DEFAULT con_false,
              p_nohits AS CHECKBOX DEFAULT con_false,
              p_edit   AS CHECKBOX DEFAULT con_false.
SELECTION-SCREEN: END   OF BLOCK b.

SELECTION-SCREEN: BEGIN OF BLOCK c WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_prog  AS CHECKBOX DEFAULT con_true,
              p_fugr  AS CHECKBOX DEFAULT con_true,
              p_cinc  AS CHECKBOX DEFAULT con_true,
              p_enh   AS CHECKBOX DEFAULT con_true,
              p_scrip AS CHECKBOX DEFAULT con_true,
              p_smart AS CHECKBOX DEFAULT con_true,
              p_adobe AS CHECKBOX DEFAULT con_true.

SELECTION-SCREEN: END   OF BLOCK c.
