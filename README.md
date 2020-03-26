# abap_Filehandler
Construction to handle Files in ABAP

The classes are designed to handle any kind of incoming/outgoing file. There are many ways to handle different filetypes: Windows and Linux/Unix files (different line wrapper), binary or text, ASCII UTF-8 odr UTF-16, frontend server or the application server?

The goal is to implement a Coding in which the developer has not to care about if it's a UTF-16 BE flatfile or a binary and if its on application server or on the frontend server.

The ZCL_FILEHANDLER* classes were originally designed for a 7.30 Kernel and later adapted to 7.00. This is why the structure is messed up. They will be deleted after they are fully converted.

The new classes are ZCL_FILE* .

Next steps are:
* Convert everything to 7.5X Syntax
* Implement the (SAP Standard) Authorization
* Implement a codepage conversion for both
* (?)Implement FTP Access
* ... your ideas?


Lets see how far we can get :)
