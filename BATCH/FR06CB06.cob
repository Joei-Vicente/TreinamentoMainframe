       IDENTIFICATION                  DIVISION.                        00010001
       PROGRAM-ID. FR06CB05.                                            00020001
      *============================================================     00030001
      *     AUTOR   : JOEI LORENTI                                      00040001
      *     EMPRESA : FOURSYS                                           00050001
      *     DATA    : 19/04/2022                                        00060001
      *     OBJETIVO:   IMPRIMIR NA SYSOUT CAMPOS DE UM REGISTRO        00070001
      *============================================================     00080001
      *ENVIRONMENT                      DIVISION.                       00081001
      *============================================================     00081101
      *============================================================*    00081201
       DATA                             DIVISION.                       00081301
      *=============================================================*   00081401
      *==============================================================*  00081501
       WORKING-STORAGE                  SECTION.                        00081601
      *=============================================================*   00081701
       01 WRK-REGISTRO.                                                 00081801
          05 WRK-CODIGO   PIC 9(05)        VALUE ZEROS.                 00081901
          05 WRK-NOME     PIC X(15)        VALUE SPACES.                00082001
          05 FILLER       PIC X(10)        VALUE SPACES.                00082102
          05 WRK-CARGO    PIC X(15)        VALUE SPACES.                00082201
          05 FILLER       PIC X(15)        VALUE SPACES.                00082402
      *==============================================================*  00082501
       PROCEDURE                        DIVISION.                       00083001
      *===============================================================* 00084001
           ACCEPT WRK-REGISTRO          FROM SYSIN.                     00085001
           DISPLAY 'CODIGO....: ' WRK-CODIGO.                           00085101
           DISPLAY 'NOME......: ' WRK-NOME.                             00085201
           DISPLAY 'CARGO.....: ' WRK-CARGO.                            00085401
           STOP RUN.                                                    00085601
