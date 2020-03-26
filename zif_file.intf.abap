interface ZIF_FILE
  public .


  constants MC_LOC_SAPGUI type ZE_FILE_LOCATION value 'S' ##NO_TEXT.
  constants MC_LOC_APPSERVER type ZE_FILE_LOCATION value 'A' ##NO_TEXT.
  constants MC_FILENAME_REPLACE type DXFILENAME value '<FILENAME>' ##NO_TEXT.
  constants MC_FILETYPE_DIR type FILETYPE value 'DIR' ##NO_TEXT.
  constants MC_FILETYPE_FILE type FILETYPE value 'FILE' ##NO_TEXT.

  methods COPY_FILE
    importing
      !I_DESTINATION type ZE_FILE_LOCATION
      !I_PATH type CSEQUENCE
    returning
      value(R_RESULT) type ref to ZIF_FILE .
  methods DELETE_FILE
    returning
      value(R_RESULT) type SY-SUBRC
    raising
      CX_SY_FILE_AUTHORITY
      CX_SY_FILE_OPEN .
  methods GET_CONTENT
    returning
      value(R_RESULT) type ref to DATA .
  methods GET_DIRECTORY
    returning
      value(R_RESULT) type STRING .
  methods GET_DIRECTORY_CONTENT
    importing
      !I_ONLY_SUBDIR type FLAG default ABAP_FALSE
      !I_ONLY_FILES type FLAG default ABAP_FALSE
      !I_BUILD_OBJECT type FLAG default ABAP_FALSE
      !I_APPEND_RETTABLE type FLAG default ABAP_FALSE
    returning
      value(R_RESULT) type ZTT_BASIC_FILE_LIST .
  methods GET_DIRECTORY_SEPERATOR
    returning
      value(R_RESULT) type CHAR01 .
  methods GET_ENCODING
    returning
      value(R_RESULT) type ABAP_ENCODING .
  methods GET_FILENAME
    returning
      value(R_RESULT) type STRING .
  methods GET_FILESIZE
    returning
      value(R_RESULT) type I .
  methods GET_FILE_EXTENSION
    returning
      value(R_RESULT) type STRING .
  methods GET_FILE_IS_BINARY
    returning
      value(R_RESULT) type ABAP_BOOL .
  methods GET_FULL_PATH
    returning
      value(R_RESULT) type STRING .
  methods GET_M_LOCATION
    returning
      value(R_RESULT) type ZE_FILE_LOCATION .
  methods GET_OPERATING_SYSTEM
    returning
      value(R_RESULT) type SY-OPSYS .
  methods MOVE_FILE
    importing
      !I_DESTINATION type ZE_FILE_LOCATION
      !I_PATH type CSEQUENCE
    returning
      value(R_RESULT) type ref to ZIF_FILE .
  methods READ_FILE
    importing
      !I_READ_AS_BINARY type FLAG default ABAP_FALSE
    exporting
      !E_FILE_LENGTH type INT4 .
  methods SET_CONTENT
    importing
      !I_CONTENT type ref to DATA .
  methods SET_DIRECTORY
    importing
      !I_DIRECTORY type STRING .
  methods SET_ENCODING
    importing
      !I_ENCODING type ABAP_ENCODING .
  methods SET_FILENAME
    importing
      !I_FILENAME type STRING .
  methods SET_FILESIZE
    importing
      !I_M_FILESIZE type I .
  methods SET_FILE_EXTENSION
    importing
      !I_FILE_EXTENSION type STRING .
  methods SET_FILE_IS_BINARY
    importing
      !I_FILE_IS_BINARY type ABAP_BOOL .
  methods SET_FULLPATH_BY_LOGIC_FILENAME
    importing
      !I_LOG_FILENAME type FILENAME-FILEINTERN
      !I_PARAM1 type CSEQUENCE
      !I_PARAM2 type CSEQUENCE
      !I_PARAM3 type CSEQUENCE
    returning
      value(R_RESULT) type SY-SUBRC .
  methods SET_FULLPATH_BY_LOGIC_PATH
    importing
      !I_LOGPATH type FILEPATH-PATHINTERN
    returning
      value(R_RESULT) type SY-SUBRC .
  methods SET_FULL_PATH
    importing
      !I_FULL_PATH type STRING .
  methods SET_M_LOCATION
    importing
      !I_M_LOCATION type ZE_FILE_LOCATION .
  methods SET_OPERATING_SYSTEM
    importing
      !I_OPERATING_SYSTEM type SY-OPSYS .
  methods WRITE_FILE
    importing
      !I_WRITE_AS_BINARY type FLAG optional
      !I_BINSIZE type INT4 optional .
endinterface.
