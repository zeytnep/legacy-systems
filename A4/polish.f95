!***************************************************************************
! CIS*3190 A4 - REVERSE POLISH in Fortran
! @author Zeynep Erdogru
! 05.04.2021
! Student ID#1047085
! Email zerdogru@uoguelph.ca
!----------------
! Assumptions
!No spaces are allowed inputting an expression. example/ 'A/   B/  C' is not allowed.
!***************************************************************************

program polish

    implicit none

    character (len = 40) :: inputStr
    character (len = 40) :: polishString
    character (len = 40) :: stack
    integer :: expression_len
    integer :: after_len
    integer :: i
    integer :: priority
    character :: yesNoFlag
    integer :: top
    
    ! print welcome message & init program variables
    call introduction(after_len, top, expression_len, yesNoFlag)

    !Loops until user chooses to quit
    do while(yesNoFlag == 'Y' .or. yesNoFlag == 'y')

        after_len = 0
        top = 0
        stack(top:top) = '%'

        write(*,*)"(Please do not use spaces) "
        write(*,*)"Input an expression you want to convert: "
        read(*,'(A)') inputStr

        expression_len = len_trim(inputStr)

        !Uses a state based parsing method where each character is examined individually
        do i=1,expression_len
            select case (inputStr(i:i))
                case ('%','+','-','*','/','^')
                    do
                        if (priority(inputStr(i:i)) > priority(stack(top:top))) then
                            call push_stack(inputStr, stack)
                            exit
                        else 
                            call pop(polishString,stack)               
                        endif
                    end do
                case ('(',')')
                    if(inputStr(i:i) == '(') then
                        call push_stack(inputStr,stack);
                    else if(inputStr(i:i) == ')') then
                        do while(priority(stack(top:top)) /= priority('('))
                            call pop(polishString,stack)
                        end do
                        top = top - 1
                    end if
                
                case ('A':'Z','a':'z','0':'9')
                    after_len = after_len + 1
                    polishString(after_len:after_len) = inputStr(i:i)
                case default
                    write(*,*)"Invalid operator!"        
            end select
        end do

        do
            if(top == 0 .or. top <0) then
                exit
            end if
            select case (stack(top:top))
                case ( '(' )
                    write(*,*) "Unmatched bracket"  
                case default
                    after_len = after_len + 1
                    polishString(after_len:after_len) = stack(top:top)
                end select 
                top = top - 1 
        end do
    
    !display converted expression to the user
    write(*,*)'Your converted expression: ', polishString(1:after_len)

    ! ask user if they want to convert another expression
    write(*,*)'Do you want to convert another expression? (Y/N)'

    ! loop until user inputs 'n' or 'N' to quit
    do
        read(*,*) yesNoFlag
        if(yesNoFlag == 'N' .or. yesNoFlag == 'N') then 
            write(*,*) 'GOODBYE...'
            call EXIT(0) !exit program if user enters n or N
        else if (yesNoFlag == 'y' .or. yesNoFlag == 'Y') then 
            exit
        else 
            ! if user enters anyhing other than n or y keep looping and prompting user
            write(*,*)'Please type Y for yes OR N for no: '
        end if
    end do

end do

contains 

!Subroutine push_stack.
!Purpose: Push an item to the stack
subroutine push_stack(inputStr, stack)

    character (len=*) :: inputStr, stack
    integer :: zey
        if(top < 40) then
            zey = top + 1
            top = zey
            stack(zey:zey) = inputStr(i:i)
        else 
            write(*,*) "Stack overflow!"
        end if;    
    return

end subroutine push_stack

!Subroutine pop.
!Purpose: Pop an item off stack and append it to the output string
subroutine pop(polishString, stack)

    character (len=*) :: polishString, stack
    integer :: zey
        if(top > 0) then
            after_len = after_len + 1
            polishString(after_len:after_len) = stack(top:top)
            zey = top - 1
            top = zey
        else
            write(*,*) "Stack underflow!"    
        end if;    
    return

end subroutine pop

! End of the main program polish
end program polish

!Subroutine introduction.
!Purpose: display introduction message to the user and init program variables
subroutine introduction(after_len, top, expression_len, yesNoFlag) 
    implicit none 

    integer :: after_len, top, expression_len
    character :: yesNoFlag

    after_len = 0
    top = 0
    expression_len = 0
    yesNoFlag = 'Y' ! init user flag

    write (*,*) ' '
    write (*,*) '------------------------------------'
    write (*,*) 'WELCOME TO REVERSE POLISH EXPRESSION CONVERTER!'
    write (*,*) '  By: Zeynep Erdogru (1047085) '
    write (*,*) '------------------------------------'
    write (*,*) ' '

end subroutine introduction

!Function priority
!Purpose: Return the priority of a specific symbol
integer function priority(my_char)

    character :: my_char

    if (my_char == ')') then 
        priority = -1
    else if (my_char == '%') then 
        priority = -1
    else if (my_char == '(') then 
        priority = 0
    else if (my_char == '+') then 
        priority = 1
    else if (my_char == '-') then 
        priority = 1
    else if (my_char == '*') then 
        priority = 2
    else if (my_char == '/') then
        priority = 2
    else if (my_char == '^') then 
        priority = 3
    end if

end function priority
