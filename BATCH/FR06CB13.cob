      *=========================================*                       00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=========================================*                       00003000
                                                                        00004000
       PROGRAM-ID.  FR01CB13.                                           00005000
                                                                        00006000
      *=======================================================*         00007000
      *     AUTOR   : JOEI LORENTI                                      00008000
      *     ANALISTA: IVAN SANCHES                                      00008100
      *     EMPRESA : FOURSYS                                           00009000
      *-------------------------------------------------------*         00010000
      *     DATA   : ZZ/ZZ/ZZZ                                          00020000
      *     OBJETIVO: GRAVACAO DA ARQUIVO FUNC                          00021000
      *=======================================================*         00022000
                                                                        00023000
      *=======================================================*         00024000
       ENVIRONMENT                               DIVISION.              00025000
      *=======================================================*         00026000
       INPUT-OUTPUT                              SECTION.               00026100
       FILE-CONTROL.                                                    00026200
             SELECT ARQPROD  ASSIGN TO ARQPROD                          00026300
                FILE STATUS  IS WRK-FS-ARQPROD.                         00026400
                                                                        00026500
             SELECT SAIARQ   ASSIGN TO SAIARQ                           00026600
                FILE STATUS  IS WRK-FS-SAIARQ.                          00026700
      *=======================================================*         00026800
       DATA                                      DIVISION.              00026900
      *=======================================================*         00027000
       FILE                                      SECTION.               00028000
       FD ARQPROD                                                       00028100
           RECORDING MODE IS F                                          00028200
           BLOCK CONTAINS 0 RECORDS.                                    00028300
       01 FD-PRODUTO  PIC X(70).                                        00028400
                                                                        00028500
       FD SAIARQ                                                        00028600
           RECORDING MODE IS F                                          00028700
           BLOCK CONTAINS 0 RECORDS.                                    00028800
       01 FD-RESUMO   PIC X(40).                                        00028900
      *---------------------------------------------------              00029000
       WORKING-STORAGE                           SECTION.               00029100
      *---------------------------------------------------              00029200
       77 WRK-FS-PRODUTO  PIC X(02) VALUE SPACES.                       00029300
       77 WRK-FS-RESUMO   PIC X(02) VALUE SPACES.                       00029400
       77 WRK-MSG         PIC X(50) VALUE SPACES.                       00029500
      *=========================================*                       00029600
       PROCEDURE DIVISION.                                              00029700
      *=========================================*                       00029800
                                                                        00029900
      *---------------------------------------------------              00030000
       0000-PRINCIPAL                     SECTION.                      00030100
      *---------------------------------------------------              00030200
           PERFORM 0100-INICIAR.                                        00030300
           PERFORM 0200-PROCESSAR UNTIL WRK-FS-PRODUTO EQUAL '10'       00030400
           PERFORM 0300-FINALIZAR.                                      00030500
      *---------------------------------------------------              00030600
       0000-99-FIM.            EXIT.                                    00030700
      *---------------------------------------------------              00030800
                                                                        00030900
       0100-INICIAR                       SECTION.                      00031000
            OPEN INPUT PRODUTO                                          00031100
                 OUTPUT RESUMO.                                         00031200
              PERFORM 0110-TESTAR-STATUS.                               00031300
       0110-TESTAR-STATUS                 SECTION.                      00031400
            IF WRK-FS-PRODUTO NOT EQUAL ZEROS                           00031500
                 MOVE ' ERRO OPEN PRODUTO ' TO WRK-MSG                  00031600
                   PERFORM 9000-TRATA-ERROS                             00031700
             END-IF.                                                    00031800
            IF WRK-FS-RESUMO NOT EQUAL ZEROS                            00031900
                 MOVE ' ERRO OPEN RESUMO ' TO WRK-MSG                   00032000
                   PERFORM 9000-TRATA-ERROS                             00033000
             END-IF.                                                    00034000
       0200-PROCESSAR                             SECTION.              00035000
             MOVE SPACES TO FD-PRODUTO                                  00035100
             READ PRODUTO                                               00036000
              IF WRK-FS-PRODUTO EQUAL ZEROS                             00037000
                  MOVE FD-PRODUTO(1:40) TO FD-RESUMO                    00038000
                                                                        00039000
                   WRITE FD-RESUMO                                      00040000
              ELSE                                                      00050000
                 MOVE ' FINAL DE ARQUIVO ' TO WRK-MSG                   00060000
                                                                        00061000
             END-IF.                                                    00062000
       0300-FINALIZAR                          SECTION.                 00063000
                   CLOSE PRODUTO                                        00064000
                   CLOSE RESUMO                                         00065000
                    MOVE 'FINAL DE PROCESSO' TO WRK-MSG                 00066000
                    PERFORM 9000-TRATA-ERROS.                           00067000
      *---------------------------------------------------              00068000
       9000-TRATA-ERROS                  SECTION.                       00069000
      *---------------------------------------------------              00070000
             DISPLAY '------------------'.                              00080000
             DISPLAY '  MENSAGEM        '  WRK-MSG.                     00090000
               STOP RUN.                                                00091000
      *--------------------------------------------------------------   00092000
       9000-99-FIM.            EXIT.                                    00093000
      *--------------------------------------------------------------   00094000
                                                                        00095000
