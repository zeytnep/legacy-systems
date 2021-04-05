*> -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
*>     File:  statnew.cob
*>     29.03.2021
*>     Name:   zeynep Erdogru
*>     Student ID: 1047085
*>     Email: zerdogru@uoguelph.ca
*>     CIS*3190 Software for legacy systems - Assignment 3
*>     @author Zeynep
*>-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

     identification division.

       program-id. statsnew.

     *> specify file control variables (dynamic input/output files)
     environment division.
       input-output section.
       file-control.
       select input-file assign to dynamic in-fname 
           organization is line sequential.
       select out-file assign to dynamic out-fname               
           organization is line sequential.

     data division.
       *> specify file characteristics and declare all variables
       file section.
       fd input-file.
           01 sample-input pic x(80).
       fd out-file.
           01 out-line pic x(80).
        
       *> declare all variables/types
       working-storage section.
       77 in-fname   pic x(30).
       77 out-fname  pic x(30).
       77 num-count picture s9999 usage is computational.
       77 feof    pic 9 value 1.
       77 i       picture s9999 usage is computational.
       77 j       picture s9999 usage is computational.
       77 aa      picture s9999 usage is computational.
       77 bb      picture s9999 usage is computational.
       77 temp               picture s9(14)v9(14) usage is computational-3.
       77 numbers-sum        picture s9(14)v9(14) usage is computational-3 value zero.
       77 standardDeviation  picture s9(14)v9(14) usage is computational-3 value zero.
       77 mean               picture s9(14)v9(14) usage is computational-3.
       77 variance           picture s9(14)v9(14) usage is computational-3 value zero.
       77 quadratic-mean     picture s9(14)v9(14) usage is computational-3 value 1.
       77 harmMean           picture s9(14)v9(14) usage is computational-3 value zero.
       77 median             picture s9(14)v9(14) usage is computational-3 value zero.

       01 array-area.
          02 num-array         picture s9(14)v9(14) usage is computational-3 occurs 1000 times.
       01 in-card.
          02 in-x              picture s9(14)v9(4).
          02 filler            picture x(62).

       01 under-line.
          02 filler            picture x(40) value '-----------------------------------'.
       01 title-line.
          02 filler            picture x(21) value '          Data Values'.
       01 data-line.
          02 filler            picture x(5) values spaces.
          02 out-x             picture -(14)9.9(4).

       01 mean-line.
          02 filler            picture x(30) value ' Mean               =   '.
          02 out-mean          picture -(14)9.9(4).
       01 staddev-line.
          02 filler            picture x(30) value ' Standard Deviation = '.
          02 out-standardDeviation picture -(14)9.9(4).
       01 variance-line.
          02 filler            picture x(30) value ' Variance           = '.
          02 out-variance      picture -(14)9.9(4).

       01 quadratic-line.
          02 filler             picture x(30) value ' Quadratic Mean     = '.
          02 out-quadratic-mean picture -(14)9.9(4).

       01 harmonicMean-line.
          02 filler            picture x(30) value ' Harmonic Mean      = '.
          02 out-harmonicMean  picture -(14)9.9(4).
       01 median-line.
          02 filler            picture x(30) value ' Median             = '.
          02 out-median        picture -(14)9.9(4).

    *> MAIN entry to the program
    procedure division.
       
       *> print welcome message and prompt user for input and output filenames
       perform introduction.
       
       *> open input file to read and open output file to write new data in it
       open input input-file, output out-file.

       write out-line from under-line after advancing 1 lines.
       write out-line from title-line after advancing 1 lines.
       write out-line from under-line after advancing 1 lines.

       body section.

       *> perform statistic calculations 
       perform statsCalc.
       
       *> Print calculated data to output file 
       perform prints.

       *> print goodbye message to the user (to the terminal)
       display '-----------------------------------------'
       display '           DONE - THANK YOU!'
       display '   Please see your results in ' out-fname
       display '-----------------------------------------'
       display ' '

       *> close files
       close input-file, out-file.
    
       stop run.
       
       *> Paragraph to calculate statistics
       statsCalc.

           *> Read numbers from the file and calculate mean
           perform getFile-loop varying num-count from 1 by 1 
               until num-count is greater than 1000 or feof = 0.
            
           compute num-count = num-count - 2.
           compute mean = numbers-sum / num-count.
           
           *> calculate Variance by calling variance-loop
           perform variance-loop varying i from 1 by 1 
               until i is greater than num-count
        
           compute variance = variance / ( num-count ).
           *> compute/ calculate Standard Deviation
           compute standardDeviation = variance ** 0.5.

           *> calculate Quadratic Mean
           perform quadric-loop varying i from 1 by 1 
               until i is greater than num-count.

           compute quadratic-mean = quadratic-mean / num-count.
           compute quadratic-mean = quadratic-mean ** 0.5.

           *> calculate Harmonic Mean
           perform harmonic-loop varying i from 1 by 1 
               until i is greater than num-count.
           
           compute harmMean = num-count / harmMean.

           perform bubblesort.
           perform median-loop.


*>>>>>> Loops
       *>>  getFile loop to read the file and compute mean.
       getFile-loop.
           read input-file into in-card 
               at end move 0 to feof.

           if feof is not equal to zero
               move in-x to num-array( num-count ), out-x

               write out-line from data-line after advancing 1 line

               compute numbers-sum = numbers-sum + num-array( num-count )

           end-if.

       *>>  variance loop to compute variance.
       variance-loop.
           compute temp = num-array( i ) - mean.
           compute temp = temp * temp.
           compute variance = variance + temp.

       *>>  quadric loop to compute quadric mean.
       quadric-loop.
           compute quadratic-mean = quadratic-mean + (num-array( i ) ** 2) .

       harmonic-loop.
           compute harmMean = harmMean + ( 1 / num-array( i )).

       median-loop.
           divide num-count by 2 giving median remainder temp.
           if temp = 0 then
               compute median = ( num-array( num-count / 2 ) + num-array(( num-count / 2) + 1 ) ) / 2
           else
               compute median = num-array( num-count / 2 )
           end-if.

*> Bubble sort algorithm
*> by Michael Wirth (citation at the end)
       bubblesort.
           perform varying i from 1 by 1 until i is greater than num-count
              compute aa = num-count - i
              perform varying j from 1 by 1 until j is greater than aa
                 compute bb = j + 1
                 if ( num-array( j ) > num-array( bb ))
                    move num-array( j ) to temp
                    move num-array( bb ) to num-array( j )
                    move temp to num-array( bb )
                 end-if
              end-perform
           end-perform.

       *> Paragraph to print introduction message and prompt user for input and output filenames
       introduction.
           display ' '.
           display '------------------------------------'.
           display '  WELCOME TO MY STATS CALCULATOR!'.
           display '   By: Zeynep Erdogru (1047085)'.
           display '------------------------------------'.
           display ' '.

           *> get user input for the input/output file names
           display 'NOTE: Please include extension in filenames (example: input.txt)'
           display ' '
           display "Enter input filename: ".
           accept in-fname.

           display "Enter output filename: ".
           accept out-fname.
           
       prints.

           write out-line from under-line after advancing 1 line.
           move mean to out-mean.
           move standardDeviation to out-standardDeviation.
           move variance to out-variance.
           move quadratic-mean to out-quadratic-mean.
           move harmMean to out-harmonicMean.
           move median to out-median.

           write out-line from mean-line after advancing 1 line.
           write out-line from staddev-line after advancing 1 line.
           write out-line from variance-line after advancing 1 line.
           write out-line from quadratic-line after advancing 1 line.
           write out-line from harmonicMean-line after advancing 1 line.
           write out-line from median-line after advancing 1 line.

*> Lines 192 - 203 are inspired from Michael Wirth's bubble sort algorithm
*> Citation: https://craftofcoding.wordpress.com/2021/03/23/coding-cobol-a-bubblesort/
*> Bubble sort algorithm, used for finding median written by Michael Wirth
