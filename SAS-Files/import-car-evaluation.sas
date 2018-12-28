/* Generated Code (IMPORT) */
/* Source File: car-evaluation.csv */
/* Source Path: /home/rileymy0/sasuser.v94 */
/* Code generated on: 12/27/18, 8:59 PM */

%web_drop_table(MRRLIB.CAR_EVAL);


FILENAME REFFILE '/home/rileymy0/sasuser.v94/car-evaluation.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=MRRLIB.CAR_EVAL;
	GETNAMES=YES;
	DATAROW=2;
	GUESSINGROWS=1000;
RUN;

PROC CONTENTS DATA=MRRLIB.CAR_EVAL; RUN;


%web_open_table(MRRLIB.CAR_EVAL);