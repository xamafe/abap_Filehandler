class ZCL_FILEHANDLER_SAPSERVER definition
  public
  inheriting from ZCL_FILEHANDLER_BASE
  final
  create public .

public section.

*"* public components of class ZCL_BC_FILEHANDLER_SAPSERVER
*"* do not include other source files here!!!
  constants MC_DESTINATION_SAP type ZE_DESTINATION value 'S' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !IV_PATH type STRING optional
    exceptions
      NOT_IMPLEMENTED .
  class-methods F4_HELP
    importing
      !IV_STARTDIR type CSEQUENCE optional
      !IV_FILEMASK type CSEQUENCE default '*'
    changing
      !CV_FULLPATH type STRING .

  methods DELETE_FILE
    redefinition .
  methods EXISTS
    redefinition .
  methods GET_DIR_CONTENT
    redefinition .
  methods GET_OS
    redefinition .
  methods GET_SEP
    redefinition .
  methods READ_FILE
    redefinition .
  methods WRITE_FILE
    redefinition .
protected section.
*"* protected components of class ZCL_BC_FILEHANDLER_SAPSERVER
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_BC_FILEHANDLER_SAPSERVER
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_FILEHANDLER_SAPSERVER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILEHANDLER_SAPSERVER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PATH                        TYPE        STRING(optional)
* | [EXC!] NOT_IMPLEMENTED
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD constructor.
  super->constructor( iv_path = iv_path ).
  mv_destination = zcl_filehandler_sapserver=>mc_destination_sap.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILEHANDLER_SAPSERVER->DELETE_FILE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_SUBRC                       TYPE        SY-SUBRC
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD delete_file.

* ohne Pfad machen wir nichts
  IF mv_fullpath IS INITIAL AND mv_name IS NOT INITIAL.
    build_fullpath( ).
  ELSEIF mv_fullpath IS INITIAL AND mv_name IS INITIAL.
    rv_subrc = 1024.
    RETURN.
  ENDIF.

  TRY.
      DELETE DATASET mv_fullpath. "löschen !
    CATCH cx_sy_file_authority.
      rv_subrc = 1.
    CATCH cx_sy_file_open.
      rv_subrc = 2.
  ENDTRY.

  IF rv_subrc IS INITIAL AND sy-subrc IS NOT INITIAL.
    rv_subrc = sy-subrc.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILEHANDLER_SAPSERVER->EXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_EXISTS                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD exists.
* Existiert die Datei?

  rv_exists = abap_true.

  OPEN DATASET mv_fullpath FOR INPUT IN TEXT MODE ENCODING DEFAULT. "zum lesen öffnen
  IF sy-subrc <> 0.
    rv_exists = abap_false.
  ENDIF.

  CLOSE DATASET mv_fullpath.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BC_FILEHANDLER_SAPSERVER=>F4_HELP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_STARTDIR                    TYPE        CSEQUENCE(optional)
* | [--->] IV_FILEMASK                    TYPE        CSEQUENCE (default ='*')
* | [<-->] CV_FULLPATH                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD f4_help.

  DATA: lv_startdir TYPE char03.

  FIELD-SYMBOLS: <startdir> TYPE any.

  IF iv_startdir IS NOT INITIAL.
    ASSIGN iv_startdir TO <startdir>.
  ELSE.
    IF iv_startdir IS INITIAL AND sy-opsys = 'Windows NT'.
      lv_startdir = 'C:\'.
    ELSEIF iv_startdir IS INITIAL.
      lv_startdir = '/'.
    ENDIF.
    ASSIGN lv_startdir TO <startdir>.
  ENDIF.

  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    EXPORTING
      directory        = <startdir>
*      filemask         = iv_filemask
    IMPORTING
      serverfile       = cv_fullpath
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILEHANDLER_SAPSERVER->GET_DIR_CONTENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PATH                        TYPE        CSEQUENCE(optional)
* | [--->] IV_ONLY_SUBDIR                 TYPE        FLAG (default =ABAP_FALSE)
* | [--->] IV_ONLY_FILES                  TYPE        FLAG (default =ABAP_FALSE)
* | [--->] IV_BUILD_OBJECT                TYPE        FLAG (default =ABAP_FALSE)
* | [--->] IV_APPEND_RETTABLE             TYPE        FLAG (default =ABAP_FALSE)
* | [<-()] RT_DIRECTORY_CONTENT           TYPE        ZBCTT_BASIC_FILE_LIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_dir_content.
* In anlehnung an: form fill_file_list von RSWATCH0

  DATA: ls_file TYPE zst_basic_file_list,
        lv_subrc TYPE sy-subrc,
        lv_path(1024),
        lv_name(1024),
        lv_length TYPE i,
        lv_owner  TYPE text80,
        lv_mtime  TYPE text80,
        lv_mode   TYPE text80,
        lv_errno  TYPE text80,
        lv_errmsg TYPE text80.

  IF iv_path IS NOT INITIAL.
    mv_path = iv_path.
*   Kürze den Pfad um eine Stelle, wenn das letzte Zeichen ein Seperator ist...
    lv_length = strlen( mv_path ) - 1.
    IF mv_path+lv_length(1) = get_sep( ).
      mv_path = mv_path(lv_length).
    ENDIF.
  ENDIF.
  replace_fullpath_variables( ).
  lv_path = mv_path.
  CHECK lv_path IS NOT INITIAL.
  CALL 'C_DIR_READ_FINISH'.            " just to be sure

* Verzeichnis öffnen
  CALL 'C_DIR_READ_START' ID 'DIR'    FIELD lv_path
                          ID 'FILE'   FIELD '*'.

  CHECK sy-subrc = 0.

  IF iv_append_rettable = abap_false.
    REFRESH rt_directory_content.
  ENDIF.

  DO.
    CLEAR ls_file.
    CALL 'C_DIR_READ_NEXT'
         ID 'TYPE'   FIELD ls_file-type
         ID 'NAME'   FIELD lv_name
         ID 'LEN'    FIELD ls_file-length
         ID 'OWNER'  FIELD lv_owner        "not used ATM
         ID 'MTIME'  FIELD lv_mtime        "not used ATM
         ID 'MODE'   FIELD lv_mode         "not used ATM
         ID 'ERRNO'  FIELD lv_errno        "not used ATM
         ID 'ERRMSG' FIELD lv_errmsg.      "not used ATM
    lv_subrc = sy-subrc.
    ls_file-name =  lv_name.
    IF lv_subrc = 5. "Fünf ist eine andere Art der Datei ...
      lv_subrc = 0.
    ENDIF.
    CASE lv_subrc.
      WHEN 0.
        CASE ls_file-type(1).
          WHEN 'F' OR 'f'.  " normal file.
            CHECK iv_only_subdir = abap_false.
            ls_file-type = mc_filetype_file.
            IF iv_build_object = abap_true.
              CREATE OBJECT ls_file-obj TYPE zcl_filehandler_sapserver
                EXPORTING
                  iv_path = |{ lv_path && get_sep( ) && ls_file-name }|.
            ENDIF.
            APPEND ls_file TO rt_directory_content.
          WHEN 'D' OR 'd'.  "Directory
            CHECK iv_only_files = abap_false.
            ls_file-type = mc_filetype_dir.
            APPEND ls_file TO rt_directory_content.
          WHEN OTHERS. " directory, device, fifo, socket,...
            CLEAR ls_file-type.
            "MOVE sap_no  TO file-useable.
        ENDCASE.

      WHEN 1.
        EXIT.
      WHEN OTHERS.                     " SY-SUBRC >= 2
*        ADD 1 TO errcnt.
*        IF errcnt > 10.
*          EXIT.
*        ENDIF.
    ENDCASE.
  ENDDO.

  CALL 'C_DIR_READ_FINISH'.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILEHANDLER_SAPSERVER->GET_OS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_OS                          TYPE        SY-OPSYS
* +--------------------------------------------------------------------------------------</SIGNATURE>
method GET_OS.
  rv_os = sy-opsys.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILEHANDLER_SAPSERVER->GET_SEP
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_SEP                         TYPE        CHAR01
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD GET_SEP.
* Das ist zwar hier sinnlos, aber es ist ein alter Reflex ALLES einzubeziehen
  CASE sy-opsys.
    WHEN 'Windows NT'.
      rv_sep = '\'.
    WHEN 'Linux'.
      rv_sep = '/'.
    WHEN 'HP-UX'.
      rv_sep = '/'.
    WHEN 'OS400'.
      rv_sep = '/'.
    WHEN OTHERS.
      rv_sep = '/'.
  ENDCASE.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILEHANDLER_SAPSERVER->READ_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_READ_AS_BINARY              TYPE        FLAG (default =ABAP_FALSE)
* | [<---] EV_LENGTH                      TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD read_file.

  DATA:
    lv_line  TYPE string,
    lv_xline TYPE xstring,
    lv_enc   TYPE sychar01,
    lv_bom   TYPE sychar01,
    lv_off   TYPE i,            "offset
    lv_cr(2) TYPE x VALUE '0D00'. "Zeilenvorschub entfernen, falls Datei aus Windows

  FIELD-SYMBOLS: <table>     TYPE table,
                 <last_byte> TYPE x,
                 <line>      TYPE any.

* Datei öffnen
  IF iv_read_as_binary = abap_true.
    OPEN DATASET mv_fullpath FOR INPUT IN BINARY MODE.
    ASSIGN lv_xline TO <line>.
    mv_as_binary = abap_true.
  ELSE.
    cl_abap_file_utilities=>check_utf8( EXPORTING
                      file_name = mv_fullpath    " Dateiname (wie bei OPEN DATASET)
                    IMPORTING
                      encoding  = lv_enc         " Encoding (Klassenkonstante)
                      bom       = lv_bom ).
    CASE lv_enc.
      WHEN cl_abap_file_utilities=>encoding_7bit_ascii. "hier UTF-7 ... :D
        OPEN DATASET mv_fullpath FOR INPUT IN TEXT MODE ENCODING NON-UNICODE WITH SMART LINEFEED.
        set_encoding( mc_codepage_iso8859_15 ).
        ASSIGN lv_line TO <line>.
        mv_as_binary = abap_false.
      WHEN cl_abap_file_utilities=>encoding_utf8. "Standard Unicodebehandlung
        IF lv_bom IS NOT INITIAL. "die BOM kann man dann getrost ignorieren
          OPEN DATASET mv_fullpath FOR INPUT IN TEXT MODE ENCODING UTF-8 AT POSITION 3 WITH SMART LINEFEED.
          mv_as_binary = abap_false.
        ELSE.
          OPEN DATASET mv_fullpath FOR INPUT IN TEXT MODE ENCODING UTF-8 WITH SMART LINEFEED.
          mv_as_binary = abap_false.
        ENDIF.
        set_encoding( mc_codepage_utf8 ).
        ASSIGN lv_line TO <line>.
      WHEN cl_abap_file_utilities=>encoding_other. "UTF-16 und ANSI landen hier
        CASE lv_bom.
          WHEN cl_abap_file_utilities=>bom_utf16_be.
            OPEN DATASET mv_fullpath FOR INPUT IN BINARY MODE.
            set_encoding( mc_codepage_utf16_be ).
            mv_as_binary = abap_true.
          WHEN cl_abap_file_utilities=>bom_utf16_le.
            OPEN DATASET mv_fullpath FOR INPUT IN BINARY MODE.
            set_encoding( mc_codepage_utf16_le ).
            mv_as_binary = abap_true.
          WHEN OTHERS.
            "Fall Back ... einfach Standard
            OPEN DATASET mv_fullpath FOR INPUT IN TEXT MODE ENCODING NON-UNICODE WITH SMART LINEFEED.
            set_encoding( mc_codepage_iso8859_15 ).
            ASSIGN lv_line TO <line>.
            mv_as_binary = abap_false.
        ENDCASE.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

  IF mt_content IS NOT BOUND.
    IF mv_as_binary = abap_true.
      CREATE DATA mt_content TYPE ztt_xfilecontent.
    ELSE.
      CREATE DATA mt_content TYPE ztt_filecontent.
    ENDIF.
  ENDIF.

  ASSIGN mt_content->* TO <table>.

* Daten laden
  CLEAR mv_filesize.
  DO.
    READ DATASET mv_fullpath INTO <line>.
    IF sy-subrc = 0.
      IF mv_as_binary = abap_true.
        mv_filesize = mv_filesize + xstrlen( <line> ).
      ELSE.
        mv_filesize = mv_filesize + strlen( <line> ).
      ENDIF.
      APPEND <line> TO <table>.
      CLEAR <line>.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

* Datei schließen
  CLOSE DATASET mv_fullpath.
  ev_length = mv_filesize.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_FILEHANDLER_SAPSERVER->WRITE_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_WRITE_AS_BINARY             TYPE        FLAG(optional)
* | [--->] IV_BINSIZE                     TYPE        INT4(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD write_file.

  DATA: lo_conv    TYPE REF TO cl_abap_conv_out_ce,
        lv_xstr    TYPE xstring,
        lv_null(2) TYPE x,
        lv_str     TYPE string.

  FIELD-SYMBOLS: <table> TYPE table,
                 <line>  TYPE any,
                 <out>   TYPE any.

* Datei öffnen
  IF iv_write_as_binary = abap_true.
    OPEN DATASET mv_fullpath FOR OUTPUT IN BINARY MODE.
*  ELSEIF mv_unicode = abap_true. "Implizit: iv_write_as_binary = abap_false.
*
  ELSE. "Implizit: mv_unicode = abap_false AND iv_write_as_binary = abap_false.
    CASE mv_encoding.
      WHEN mc_codepage_iso8859_15  .
        OPEN DATASET mv_fullpath FOR OUTPUT ENCODING NON-UNICODE IN TEXT MODE.
      WHEN mc_codepage_utf8        .
        OPEN DATASET mv_fullpath FOR OUTPUT ENCODING DEFAULT IN TEXT MODE.
      WHEN mc_codepage_utf16_be OR mc_codepage_utf16_le
        OR mc_codepage_utf32_be OR mc_codepage_utf32_le.
        OPEN DATASET mv_fullpath FOR OUTPUT IN BINARY MODE.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

* Daten laden
  ASSIGN mt_content->* TO <table>.
  LOOP AT <table> ASSIGNING <line>.
    AT FIRST.
      IF iv_write_as_binary = abap_false.
        CASE mv_encoding.
          WHEN mc_codepage_utf16_be.
            TRANSFER cl_abap_char_utilities=>byte_order_mark_big TO mv_fullpath.
          WHEN mc_codepage_utf16_le.
            TRANSFER cl_abap_char_utilities=>byte_order_mark_little TO mv_fullpath.
          WHEN mc_codepage_utf32_be.
            TRANSFER lv_null TO mv_fullpath.
            TRANSFER cl_abap_char_utilities=>byte_order_mark_big TO mv_fullpath.
          WHEN mc_codepage_utf32_le .
            TRANSFER cl_abap_char_utilities=>byte_order_mark_little TO mv_fullpath.
            TRANSFER lv_null TO mv_fullpath.
          WHEN OTHERS.
            "Dont do anything
        ENDCASE.
      ENDIF.
    ENDAT.
    IF    iv_write_as_binary = abap_false
       AND ( mv_encoding = mc_codepage_utf16_be
          OR mv_encoding = mc_codepage_utf16_le
          OR mv_encoding = mc_codepage_utf32_be
          OR mv_encoding = mc_codepage_utf32_le ).
      lo_conv = cl_abap_conv_out_ce=>create( encoding = mv_encoding ).
      lv_str = <line> && cl_abap_char_utilities=>cr_lf. "Die gehen sonst verloren
      lo_conv->write( data = lv_str ).
      lv_xstr = lo_conv->get_buffer( ).
      ASSIGN lv_xstr TO <out>.
    ELSE.
      ASSIGN <line> TO <out>.
    ENDIF.
    TRANSFER <out> TO mv_fullpath.
  ENDLOOP.

* Datei schließen
  CLOSE DATASET mv_fullpath.

ENDMETHOD.
ENDCLASS.
