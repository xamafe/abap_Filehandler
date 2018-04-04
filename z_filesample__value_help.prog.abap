*&---------------------------------------------------------------------*
*& Report Z_FILESAMPLE__VALUE_HELP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_filesample__value_help.

PARAMETERS:
  p_path TYPE string LOWER CASE,
  p_gui  RADIOBUTTON GROUP a,
  p_sap  RADIOBUTTON GROUP a.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
* Read Parameter from SelScreen
  zcl_tools=>get_parameter_value_of_dynpro( EXPORTING iv_field  = 'P_GUI'
                                                      iv_report = sy-repid
                                                      iv_dynnr  = sy-dynnr
                                            IMPORTING
                                                               ev_parameter = p_gui ).
* Read Parameter from SelScreen
  zcl_tools=>get_parameter_value_of_dynpro( EXPORTING iv_field  = 'P_SAP'
                                                      iv_report = sy-repid
                                                      iv_dynnr  = sy-dynnr
                                            IMPORTING
                                                      ev_parameter = p_sap ).
* Call F4 - help
  zcl_filehandler_base=>f4_help_all( EXPORTING
                                        iv_file_from_frontend  = p_gui
                                        iv_file_from_sapserver = p_sap
                                      CHANGING cv_fullpath = p_path ).

**********************************************************************
START-OF-SELECTION.
**********************************************************************

  WRITE: / 'Done'.
