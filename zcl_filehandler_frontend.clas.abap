CLASS zcl_filehandler_frontend DEFINITION
  PUBLIC
  INHERITING FROM zcl_filehandler_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
*"* public components of class ZCL_BC_FILEHANDLER_FRONTEND
*"* do not include other source files here!!!

    TYPES:
      BEGIN OF mts_drives,
        name(2), "letter + :
        type    TYPE string,
      END OF mts_drives .
    TYPES:
      mtt_drives TYPE STANDARD TABLE OF mts_drives .

    CONSTANTS mc_destination_frontend TYPE ze_destination VALUE 'F'. "#EC NOTEXT
    DATA mt_drives TYPE mtt_drives .

    METHODS constructor
      IMPORTING
        !iv_path TYPE string OPTIONAL
      EXCEPTIONS
        not_implemented .
    METHODS get_drives .
    CLASS-METHODS f4_help
      CHANGING
        !cv_fullpath TYPE string .

    METHODS exists
        REDEFINITION .
    METHODS get_dir_content
        REDEFINITION .
    METHODS get_os
        REDEFINITION .
    METHODS get_sep
        REDEFINITION .
    METHODS read_file
        REDEFINITION .
    METHODS write_file
        REDEFINITION .
    METHODS delete_file
        REDEFINITION .
  PROTECTED SECTION.
*"* protected components of class ZCL_BC_FILEHANDLER_FRONTEND
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_BC_FILEHANDLER_FRONTEND
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_filehandler_frontend IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILEHANDLER_FRONTEND->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PATH                        TYPE        STRING(optional)
* | [EXC!] NOT_IMPLEMENTED
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    super->constructor( iv_path = iv_path ).
    mv_destination = zcl_filehandler_frontend=>mc_destination_frontend.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILEHANDLER_FRONTEND->DELETE_FILE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_SUBRC                       TYPE        SY-SUBRC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD delete_file.

* no filepath? No file!
    IF mv_fullpath IS INITIAL AND mv_name IS NOT INITIAL.
      build_fullpath( ).
    ELSEIF mv_fullpath IS INITIAL AND mv_name IS INITIAL.
      rv_subrc = 1024.
      RETURN.
    ENDIF.

    cl_gui_frontend_services=>file_delete(
       EXPORTING
         filename             = mv_fullpath
       CHANGING
         rc                   = rv_subrc
       EXCEPTIONS
         file_delete_failed   = 1
         cntl_error           = 2
         error_no_gui         = 3
         file_not_found       = 4
         access_denied        = 5
         unknown_error        = 6
         not_supported_by_gui = 7
         wrong_parameter      = 8
         OTHERS               = 9 ).

    IF rv_subrc IS INITIAL AND sy-subrc IS NOT INITIAL.
      rv_subrc = sy-subrc.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILEHANDLER_FRONTEND->EXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_EXISTS                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD exists.
    rv_exists = cl_gui_frontend_services=>file_exist( file = mv_fullpath ). " File under test
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BC_FILEHANDLER_FRONTEND=>F4_HELP
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CV_FULLPATH                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD f4_help.

    DATA: lt_files TYPE filetable,
          lv_rc    TYPE i.

    FIELD-SYMBOLS: <file> TYPE file_table.

    cl_gui_frontend_services=>file_open_dialog( EXPORTING
*                                                 window_title            =
*                                                 default_extension       =
*                                                 default_filename        =
*                                                 file_filter             =
*                                                 with_encoding           =
*                                                 initial_directory       =
                                                    multiselection          = abap_false
                                                CHANGING
                                                    file_table              = lt_files
                                                    rc                      = lv_rc
*                                                 user_action             =
                                               ).
* no input?
    IF lv_rc < 1.
      RETURN.
    ENDIF.

* Read first file
    READ TABLE lt_files ASSIGNING <file> INDEX 1.
    IF <file> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    cv_fullpath = <file>.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILEHANDLER_FRONTEND->GET_DIR_CONTENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PATH                        TYPE        CSEQUENCE(optional)
* | [--->] IV_ONLY_SUBDIR                 TYPE        FLAG (default =ABAP_FALSE)
* | [--->] IV_ONLY_FILES                  TYPE        FLAG (default =ABAP_FALSE)
* | [--->] IV_BUILD_OBJECT                TYPE        FLAG (default =ABAP_FALSE)
* | [--->] IV_APPEND_RETTABLE             TYPE        FLAG (default =ABAP_FALSE)
* | [<-()] RT_DIRECTORY_CONTENT           TYPE        ZBCTT_BASIC_FILE_LIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_dir_content.

    DATA: lt_filetable TYPE rstt_t_files,
          lv_path      TYPE string,
          lv_cnt       TYPE i.

    FIELD-SYMBOLS: <intfile> TYPE file_info,
                   <extfile> TYPE zst_basic_file_list.

    IF iv_path IS NOT INITIAL.
      lv_path = iv_path.
    ELSE.
      lv_path = mv_path.
    ENDIF.
    CHECK lv_path IS NOT INITIAL.

* check for directory
    IF cl_gui_frontend_services=>directory_exist( lv_path ) <> abap_true.
      EXIT.
    ENDIF.

* get files
    cl_gui_frontend_services=>directory_list_files(
         EXPORTING
           directory          = lv_path                " Suchverzeichnis
             files_only       = iv_only_files          " Gibt nur Dateien zurück, keine Verzeichnisse
             directories_only = iv_only_subdir         " Gibt nur Verzeichnisse zurück, keine Dateien
         CHANGING
             file_table       = lt_filetable           " Zurückgegebene Tabelle mit gefundenen Dateinamen
             count            = lv_cnt              ). " Anzahl Dateien / Verzeichnisse gefunden

* refresh table?
    IF iv_append_rettable = abap_false.
      REFRESH rt_directory_content.
    ENDIF.

* Convert return
    LOOP AT lt_filetable ASSIGNING <intfile>.
      APPEND INITIAL LINE TO rt_directory_content ASSIGNING <extfile>.
      <extfile>-name   = <intfile>-filename  .
      <extfile>-length = <intfile>-filelength.
      IF <intfile>-isdir = 1. "directory found
        <extfile>-type = mc_filetype_dir.
      ELSE.
        <extfile>-type = mc_filetype_file.
      ENDIF.
      CHECK iv_build_object = abap_true.
      CREATE OBJECT <extfile>-obj TYPE zcl_filehandler_frontend
        EXPORTING
          iv_path = |{ iv_path && <extfile>-name }|.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILEHANDLER_FRONTEND->GET_DRIVES
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_drives.
* Get all drives ...
    DATA: lv_drive(2),
          lv_index TYPE sy-index.

    FIELD-SYMBOLS: <drive> TYPE mts_drives.

    DO.
      lv_index = sy-index - 1.
      IF lv_index >= strlen( sy-abcde ) .
        EXIT.
      ENDIF.
*   Read all drives
      lv_drive = |{ sy-abcde+lv_index(1) }:|.
      IF cl_gui_frontend_services=>directory_exist( |{ lv_drive }| ) = abap_true.   " Verzeichnisname
        APPEND INITIAL LINE TO mt_drives ASSIGNING <drive>.
        <drive>-name = lv_drive.
*     Get type --> see bottom
        cl_gui_frontend_services=>get_drive_type( EXPORTING
                                                    drive      = |{ <drive>-name }|   " Laufwerk
                                                  CHANGING
                                                    drive_type = <drive>-type         " Laufwerktyp
                                                  EXCEPTIONS
                                                    OTHERS   = 5 ).
        cl_gui_cfw=>flush( ).
        IF sy-subrc <> 0.
          <drive>-type = 'ERROR'.
        ENDIF.
      ENDIF.
    ENDDO.


*REMOVABLE
*A drive IS classified AS removable IF it can be removed FROM the computer WHILE the computer IS running.
* Funktioniert nicht sauber mit USB - Sticks
*FIXED
*A fixed drive cannot be removed DURING normal operation OF the computer
*
*REMOTE
*This VALUE IS returned FOR network shares
*
*CDROM
*The SPECIFIED drive IS a CD-ROM drive
*
*RAMDISK
*This IS returned FOR RAM disks

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILEHANDLER_FRONTEND->GET_OS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_OS                          TYPE        SY-OPSYS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_os.
*cl_gui_frontend_services=>get_platform(
*RECEIVING
**    platform             =     " Gibt die Plattform zurück
**  EXCEPTIONS
**    error_no_gui         = 1
**    cntl_error           = 2
**    not_supported_by_gui = 3
**    others               = 4
*).
*IF sy-subrc <> 0.
** MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILEHANDLER_FRONTEND->GET_SEP
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_SEP                         TYPE        CHAR01
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_sep.
    cl_gui_frontend_services=>get_file_separator( CHANGING file_separator = rv_sep ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILEHANDLER_FRONTEND->READ_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_READ_AS_BINARY              TYPE        FLAG (default =ABAP_FALSE)
* | [<---] EV_LENGTH                      TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD read_file.
    DATA: lv_filetype(10),
          lv_codepage TYPE abap_encoding .
    FIELD-SYMBOLS: <table> TYPE table.

* Content
    IF mt_content IS NOT BOUND.
      IF iv_read_as_binary = abap_true.
        CREATE DATA mt_content TYPE ztt_xfilecontent.
      ELSE.
        CREATE DATA mt_content TYPE ztt_filecontent.
      ENDIF.
    ENDIF.

    ASSIGN mt_content->* TO <table>.

* binary or text?
    IF iv_read_as_binary = abap_true.
      lv_filetype = 'BIN'.
      mv_as_binary = abap_true.
    ELSE.
      lv_filetype = 'ASC'.
      mv_as_binary = abap_false.
    ENDIF.

    cl_gui_frontend_services=>gui_upload(
                   EXPORTING
                     filename                = mv_fullpath
                     filetype                = lv_filetype
*                   has_field_separator     = SPACE
*                   header_length           = 0
*                   read_by_line            = 'X'
*                   dat_mode                = SPACE
                     codepage                = mv_encoding
*                   ignore_cerr             = ABAP_TRUE
*                   replacement             = '#'
                     virus_scan_profile      = space
*                   isdownload              = SPACE
                   IMPORTING
                     filelength              = mv_filesize
*                   header                  =
                   CHANGING
                     data_tab                = <table>
*                   isscanperformed         = SPACE
                   EXCEPTIONS
                     file_open_error         = 1
                     file_read_error         = 2
                     no_batch                = 3
                     gui_refuse_filetransfer = 4
                     invalid_type            = 5
                     no_authority            = 6
                     unknown_error           = 7
                     bad_data_format         = 8
                     header_not_allowed      = 9
                     separator_not_allowed   = 10
                     header_too_long         = 11
                     unknown_dp_error        = 12
                     access_denied           = 13
                     dp_out_of_memory        = 14
                     disk_full               = 15
                     dp_timeout              = 16
                     not_supported_by_gui    = 17
                     error_no_gui            = 18
                     OTHERS                  = 19
      ).
    IF sy-subrc <> 0.

    ENDIF.

    ev_length = mv_filesize.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILEHANDLER_FRONTEND->WRITE_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_WRITE_AS_BINARY             TYPE        FLAG(optional)
* | [--->] IV_BINSIZE                     TYPE        INT4(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD write_file.
    DATA: lv_filetype(10),
          lv_codepage TYPE abap_encoding .

    FIELD-SYMBOLS: <table> TYPE table.

* binary or text?
    IF iv_write_as_binary = abap_true.
      lv_filetype = 'BIN'.
    ELSE.
      lv_filetype = 'ASC'.
    ENDIF.

* content
    ASSIGN mt_content->* TO <table>.

    cl_gui_frontend_services=>gui_download(
          EXPORTING
            bin_filesize              = iv_binsize
            filename                  = mv_fullpath
            filetype                  = lv_filetype
*          append                    = SPACE
*          write_field_separator     = SPACE
*          header                    = '00'
*          trunc_trailing_blanks     = SPACE
*          write_lf                  = 'X'
*          col_select                = SPACE
*          col_select_mask           = SPACE
*          dat_mode                  = SPACE
*          confirm_overwrite         = SPACE
*          no_auth_check             = SPACE
            codepage                  = mv_encoding
*          ignore_cerr               = ABAP_TRUE
*          replacement               = '#'
            write_bom                 = mv_bom
*          trunc_trailing_blanks_eol = 'X'
*          wk1_n_format              = SPACE
*          wk1_n_size                = SPACE
*          wk1_t_format              = SPACE
*          wk1_t_size                = SPACE
*          show_transfer_status      = 'X'
*          fieldnames                =
*          write_lf_after_last_line  = 'X'
*          virus_scan_profile        = '/SCET/GUI_DOWNLOAD'
*        IMPORTING
*          filelength                =
          CHANGING
            data_tab                  = <table>
*        EXCEPTIONS
*          file_write_error          = 1
*          no_batch                  = 2
*          gui_refuse_filetransfer   = 3
*          invalid_type              = 4
*          no_authority              = 5
*          unknown_error             = 6
*          header_not_allowed        = 7
*          separator_not_allowed     = 8
*          filesize_not_allowed      = 9
*          header_too_long           = 10
*          dp_error_create           = 11
*          dp_error_send             = 12
*          dp_error_write            = 13
*          unknown_dp_error          = 14
*          access_denied             = 15
*          dp_out_of_memory          = 16
*          disk_full                 = 17
*          dp_timeout                = 18
*          file_not_found            = 19
*          dataprovider_exception    = 20
*          control_flush_error       = 21
*          not_supported_by_gui      = 22
*          error_no_gui              = 23
*          others                    = 24
            ).
    IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
