*&---------------------------------------------------------------------*
*& Report Z_FILESAMPLE__WRITE_FILE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_filesample__write_file.

PARAMETERS: p_path TYPE string LOWER CASE.

DATA:
  go_file TYPE REF TO zcl_filehandler_frontend,
  gt_itab TYPE STANDARD TABLE OF char512.

FIELD-SYMBOLS: <line> TYPE char512.

*Create File
APPEND INITIAL LINE TO gt_itab ASSIGNING <line>.
<line> = 'Hello'.
APPEND INITIAL LINE TO gt_itab ASSIGNING <line>.
<line> = 'World'.

CREATE OBJECT go_file
  EXPORTING
    iv_path = p_path.

GET REFERENCE OF gt_itab INTO go_file->mt_content.

* Optional!
go_file->set_encoding( zcl_filehandler_frontend=>mc_codepage_utf16_be ).

go_file->write_file( ).
