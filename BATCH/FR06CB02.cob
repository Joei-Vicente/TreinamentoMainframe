       IDENTIFICATION                  DIVISION.                        00010000
       PROGRAM-ID. FR06CB01.                                            00020000
      *============================================================     00030000
      *     AUTOR   : JOEI LORENTI                                      00040000
      *     EMPRESA : FOURSYS                                           00050000
      *     DATA    : 18/04/2022                                        00060000
      *     OBJETIVO: TESTE DE COMPILACAO                               00070000
      *============================================================     00080000
      *ENVIRONMENT                      DIVISION.                       00081000
       DATA                             DIVISION.                       00082000
       WORKING-STORAGE                  SECTION.                        00083000
       77 WRK-NOME     PICTURE X(15).                                   00084001
       PROCEDURE                        DIVISION.                       00090000
           MOVE 'JOEI LORENTI' TO WRK-NOME.                             00091009
                                                                        00093009
           DISPLAY 'MEU SEGUNDO PROGRAMA - FR06CB02'.                   00100000
           DISPLAY 'FEITO POR...... - ' WRK-NOME.                       00101009
           STOP RUN.                                                    00110000
