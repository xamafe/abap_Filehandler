CLASS zcl_file_appserver DEFINITION
  PUBLIC
  CREATE PRIVATE
  INHERITING FROM zcl_file__base
  GLOBAL FRIENDS zcl_file .

  PUBLIC SECTION.
    METHODS: zif_file~copy_file REDEFINITION,
             zif_file~delete_file REDEFINITION,
             zif_file~get_directory_content REDEFINITION,
             zif_file~get_directory_seperator REDEFINITION,
             zif_file~move_file REDEFINITION,
             zif_file~read_file REDEFINITION,
             zif_file~write_file REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS value_help_for_appserver RETURNING VALUE(r_result) TYPE string.
ENDCLASS.



CLASS zcl_file_appserver IMPLEMENTATION.
  METHOD value_help_for_appserver.
    DATA: startdirectory TYPE char03.

    IF sy-opsys = 'Windows NT'.
      startdirectory = 'C:\'.
    ELSE.
      startdirectory = '/'.
    ENDIF.

    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
      EXPORTING
        directory        = startdirectory
*       filemask         = iv_filemask
      IMPORTING
        serverfile       = r_result
      EXCEPTIONS
        canceled_by_user = 1
        OTHERS           = 2.

  ENDMETHOD.

  METHOD zif_file~copy_file.

  ENDMETHOD.

  METHOD zif_file~delete_file.

  ENDMETHOD.

  METHOD zif_file~get_directory_content.

  ENDMETHOD.

  METHOD zif_file~get_directory_seperator.

  ENDMETHOD.

  METHOD zif_file~move_file.

  ENDMETHOD.

  METHOD zif_file~read_file.

  ENDMETHOD.

  METHOD zif_file~write_file.

  ENDMETHOD.

ENDCLASS.
