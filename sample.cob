//HERC01S  JOB (COBOL),'COBOL PROGRAM',CLASS=A,MSGCLASS=H,              00000100
//             REGION=8M,TIME=1440,                                     00000200
//             MSGLEVEL=(1,1),                                          00000300
//             NOTIFY=HERC01                                            00000400
//SAMPLE   EXEC COBUCG,                                                 00000500
//             PARM.COB='FLAGW,LOAD,SUPMAP,SIZE=2048K,BUF=1024K'        00000600
//COB.SYSPUNCH DD DUMMY                                                 00000700
//COB.SYSIN DD *                                                        00000800
      ********************************************************          00000900
      * A SIMPLE COBOL PROGRAM I USE TO TEST STUFF AS I LEARN.          00001000
      ********************************************************          00001100
       IDENTIFICATION DIVISION.                                         00001200
       PROGRAM-ID. SAMPLE.                                              00001300
       AUTHOR. KRIS W KEENER.                                           00001400
       INSTALLATION.   THE LAB.                                         00001500
       DATE-WRITTEN.   NOVEMBER 11 2021.                                00001600
       DATE-COMPILED.  NOVEMBER 11 2021.                                00001700
       SECURITY. HOME USE ONLY.                                         00001800
       REMARKS. THIS PROGRAM IS OF NO USE TO ANYONE, REAL               00001900
           OR IMAGINED.                                                 00002000
      *>                                                                00002100
      *>                                                                00002200
       ENVIRONMENT DIVISION.                                            00002300
      **                                                                00002400
       CONFIGURATION SECTION.                                           00002500
       SOURCE-COMPUTER.    IBM-370.                                     00002600
       OBJECT-COMPUTER.    IBM-370.                                     00002700
      **                                                                00002800
       INPUT-OUTPUT SECTION.                                            00002900
       FILE-CONTROL.                                                    00003000
           SELECT ACCTFILE, ASSIGN TO UT-S-ACCTFILE.                    00003100
           SELECT PRINTFILE, ASSIGN TO UT-S-SYSOUT.                     00003200
      *>                                                                00003300
      *>                                                                00003400
       DATA DIVISION.                                                   00003500
      **                                                                00003600
       FILE SECTION.                                                    00003700
       FD  ACCTFILE, RECORDING MODE IS F,                               00003800
           RECORD CONTAINS 37 CHARACTERS,                               00003900
           BLOCK CONTAINS 100 RECORDS,                                  00004000
           LABEL RECORDS ARE STANDARD,                                  00004100
           DATA  RECORD IS ACCTREC.                                     00004200
       01  ACCTREC.                                                     00004300
           02  CUSTOMER-NUMBER    PIC 9(9).                             00004400
           02  CUSTOMER-NAME      PIC X(21).                            00004500
           02  BILLING-AMOUNT     PIC 9(5)V99.                          00004600
       FD  PRINTFILE, RECORDING MODE IS F,                              00004700
           LABEL RECORDS ARE OMITTED,                                   00004800
           DATA RECORD IS PRINTOUT.                                     00004900
       01  PRINTOUT       PIC X(133).                                   00005000
      **                                                                00005100
       WORKING-STORAGE SECTION.                                         00005200
       01  PRINTDETAIL.                                                 00005300
           02  FILLER                  PIC X VALUE SPACE.               00005400
           02  PRINT-CUSTOMER-NUMBER   PIC 9(9).                        00005500
           02  FILLER                  PIC XXX VALUE SPACES.            00005600
           02  PRINT-CUSTOMER-NAME     PIC X(21).                       00005700
           02  FILLER                  PIC XX VALUE SPACES.             00005800
           02  PRINT-BILLING-AMOUNT    PIC $ZZ,ZZZ.99.                  00005900
      *>                                                                00006000
      *>                                                                00006100
       PROCEDURE DIVISION.                                              00006200
           OPEN INPUT ACCTFILE, OUTPUT PRINTFILE.                       00006300
           MOVE SPACES TO PRINTOUT.                                     00006400
           WRITE PRINTOUT AFTER POSITIONING 0 LINES.                    00006500
       READ-A-CARD.                                                     00006600
           READ ACCTFILE, AT END GO TO END-OF-JOB.                      00006700
           MOVE CUSTOMER-NUMBER TO PRINT-CUSTOMER-NUMBER.               00006800
           MOVE CUSTOMER-NAME TO PRINT-CUSTOMER-NAME.                   00006900
           MOVE BILLING-AMOUNT TO PRINT-BILLING-AMOUNT.                 00007000
           WRITE PRINTOUT FROM PRINTDETAIL AFTER POSITIONING            00007100
           1 LINES.                                                     00007200
           GO TO READ-A-CARD.                                           00007300
       END-OF-JOB.                                                      00007400
           DISPLAY '** EOF ON SYSIN **'.                                00007500
           CLOSE ACCTFILE, PRINTFILE.                                   00007600
           STOP RUN.                                                    00007700
      **                                                                00007800
/*                                                                      00007900
//COB.SYSLIB DD DSNAME=SYS1.COBLIB,DISP=SHR                             00008000
//GO.SYSOUT DD SYSOUT=*                                                 00008100
//GO.CEEDUMP DD SYSOUT=*                                                00008200
//GO.ACCTFILE DD DISP=SHR,DSN=HERC01.SOURCE.SAMPDATA,                   00008300
//            DCB=(DSORG=PS,LRECL=37,BLKSIZE=3700,EROPT=ABE)            00008400
//SYSIN DD *                                                            00008500
/*                                                                      00008600
/&                                                                      00008700
