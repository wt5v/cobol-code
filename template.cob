//HERC01S  JOB (COBOL),'COBOL TEMPLATE',CLASS=A,MSGCLASS=H,             00000100
//             REGION=8M,TIME=1440,                                     00000200
//             MSGLEVEL=(1,1)                                           00000300
//TEMPLATE EXEC COBUCG,                                                 00000400
//             PARM.COB='FLAGW,LOAD,SUPMAP,SIZE=2048K,BUF=1024K'        00000500
//COB.SYSPUNCH DD DUMMY                                                 00000600
//COB.SYSIN DD *                                                        00000700
      ********************************************************          00000800
      * A COBOL PROGRAM TEMPLATE                                        00000900
      ********************************************************          00001000
       IDENTIFICATION DIVISION.                                         00001100
       PROGRAM-ID. TEMPLATE.                                            00001200
       AUTHOR. KRIS W KEENER.                                           00001300
       INSTALLATION.   THE LAB.                                         00001400
       DATE-WRITTEN.   NOVEMBER 11 2021.                                00001500
       DATE-COMPILED.  NOVEMBER 11 2021.                                00001600
       SECURITY. HOME USE ONLY.                                         00001700
       REMARKS. THIS PROGRAM IS OF NO USE TO ANYONE REAL                00001800
           OR IMAGINED.                                                 00001900
      *>                                                                00002000
      *>                                                                00002100
       ENVIRONMENT DIVISION.                                            00002200
      **                                                                00002300
       CONFIGURATION SECTION.                                           00002400
       SOURCE-COMPUTER.    IBM-370.                                     00002500
       OBJECT-COMPUTER.    IBM-370.                                     00002600
      **                                                                00002700
       INPUT-OUTPUT SECTION.                                            00002800
       FILE-CONTROL.                                                    00002900
      *>                                                                00003000
      *>                                                                00003100
       DATA DIVISION.                                                   00003200
      **                                                                00003300
       FILE SECTION.                                                    00003400
      **                                                                00003500
       WORKING-STORAGE SECTION.                                         00003600
      *>                                                                00003700
      *>                                                                00003800
       PROCEDURE DIVISION.                                              00003900
           STOP RUN.                                                    00004000
      **                                                                00004100
/*                                                                      00004200
//COB.SYSLIB DD DSNAME=SYS1.COBLIB,DISP=SHR                             00004300
//GO.SYSOUT DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=161,BLKSIZE=16100)         00004400
//GO.SYSIN DD *                                                         00004500
/*                                                                      00004600
/&                                                                      00004700
