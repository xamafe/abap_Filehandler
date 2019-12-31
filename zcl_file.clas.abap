CLASS zcl_file DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS create IMPORTING i_location      TYPE ze_file_location
                                   i_full_path     TYPE string
                         RETURNING VALUE(r_return) TYPE REF TO zif_file.
    CLASS-METHODS get_location IMPORTING i_loc_sapgui    TYPE abap_bool OPTIONAL
                                         i_loc_appserver TYPE abap_bool OPTIONAL
                               RETURNING VALUE(r_return) TYPE ze_file_location.
    CLASS-METHODS value_help IMPORTING i_loc_sapgui    TYPE abap_bool OPTIONAL
                                       i_loc_appserver TYPE abap_bool OPTIONAL
                             RETURNING VALUE(r_return) TYPE string.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_file IMPLEMENTATION.


  METHOD create.
    CASE i_location.
      WHEN zif_file=>mc_loc_sapgui.
        CREATE OBJECT r_return TYPE zcl_file_sapgui.
      WHEN zif_file=>mc_loc_appserver.
        CREATE OBJECT r_return TYPE zcl_file_appserver.
      WHEN OTHERS.
        "TODO RAISE Exception
        EXIT.
    ENDCASE.
    IF i_full_path IS NOT INITIAL.
      r_return->set_full_path( i_full_path ).
    ENDIF.
  ENDMETHOD.

  METHOD get_location.
    CASE abap_true.
      WHEN i_loc_sapgui.
        r_return = zif_file=>mc_loc_sapgui.
      WHEN i_loc_appserver.
        r_return = zif_file=>mc_loc_appserver.
      WHEN OTHERS.
        r_return = abap_false.
    ENDCASE.

  ENDMETHOD.


  METHOD value_help.
    CASE abap_true.
      WHEN i_loc_sapgui.
        r_return = zcl_file_sapgui=>value_help_for_sapgui( ).
      WHEN i_loc_appserver.
        r_return = zcl_file_appserver=>value_help_for_appserver( ).
      WHEN OTHERS.
        "TODO Exception
    ENDCASE.
  ENDMETHOD.


ENDCLASS.
