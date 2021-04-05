!Zeynep Erdogru
!Student ID: 1047085
!zerdogru@uoguelph.ca

! "wrapper" program to call the subroutines and perform I/O for subroutine inputs
program fire 
    implicit none 

    !Variable declarations
    real :: drybulb, wetbulb, precip, wind, buo_index, drying_factor, ffuelm, adfuelm, grass, timber, fload
    integer :: isnow, herbState

    !invoke subroutines with "call" statement
    call input(drybulb,wetbulb,isnow,precip,wind,buo_index,herbState)
    call danger(drybulb,wetbulb,isnow,precip,wind,buo_index,drying_factor,ffuelm,adfuelm,grass,timber,fload, herbState)
    !Output of calculated info from DANGER
    call output(buo_index,ffuelm,adfuelm,timber,fload, grass)
end 

subroutine danger(drybulb,wetbulb,isnow,precip,wind,buo_index,drying_factor,ffuelm,adfuelm,grass,timber,fload, herbState)
    implicit none 

    !Variable declarations
    real :: drybulb, wetbulb, precip, wind, buo_index, drying_factor, ffuelm, adfuelm, grass, timber, fload, difference
    integer :: isnow, i, z, flag, herbState

    real :: a(4)
    real :: b(4)
    real :: c(3)
    real :: d(6)
    a = (/ -0.185900, -0.85900, -0.059660, -0.077373 /)
    b = (/30.0, 19.2, 13.8, 22.5/)
    c = (/4.5, 12.5, 27.5/)
    d = (/16.0, 10.0, 7.0, 5.0, 4.0, 3.0/)
    ffuelm = 99.
    adfuelm = 99.
    drying_factor = 0.
    fload = 0.
    flag =0

    !if there is NO snow on the ground
    if (isnow == 0) then 
        difference = drybulb - wetbulb
        !start of myLoop
        myLoop: do i =1, 3
            !if the condition is correct break out of the loop
            if ((difference - c(i))<0 .or. (difference - c(i)) == 0) then 
                flag = 1
                exit myLoop
            end if
        end do myLoop

        !we broke out of the loop
        if (flag == 1) then 
            ffuelm = b(i)*exp(a(i)*difference)
        !loop ended naturally
        else if(flag ==0) then
            i =4
            ffuelm = b(i)*exp(a(i)*difference)
        end if 
        flag =0
        !now find the drying factor for the day
        secondLoop: do z=1, 6
            if ( (ffuelm - d(z)) >0) then
                flag =1
                exit secondLoop
            end if
        end do secondLoop
        !we broke out of the loop
        if (flag == 1) then 
            drying_factor = z-1
        !loop ended naturally
        else if (flag == 0) then
            drying_factor =7
        end if
        !test to see if the fine fuel moisture is one or less
        !if fine fuel moisture is ==1 or <1 we set it to =1
        if ( (ffuelm -1.) <0) then 
            ffuelm = 1.
        end if
        !add 5 percent fine fuel moisture for each herb stage >1
        ffuelm = ffuelm + (herbState-1) *5.
        !must adjust the bui for percip before adding the drying factor
        if ((precip - .1) >0) then
            buo_index= -50. * alog(1.-(1.-exp(-buo_index/50.))*exp(-1.175*(precip-.1)))
            if (buo_index <0) then 
                buo_index = 0.0
            end if
        end if
        buo_index = buo_index + drying_factor
        !adjust the grass spread index for heavy fuel lags
        !the result will be the timber spread index
        adfuelm = .9 * ffuelm + .5 + 9.5 * exp(-buo_index/50.)
        !test to see if the fuel moistures are > 30%
        !if they are set their index values =1
        if (((adfuelm -30.) == 0) .or. ((adfuelm -30.) > 0)) then 
            if ((ffuelm - 30.) <0) then
                timber = 1.
                !test to see if the wind speed is > 14 mph
                if ((wind - 14. ) < 0) then 
                    grass = .01312*(wind+6.) * (33.-ffuelm)**1.65 - 3.
                    if ((timber - 1.) >0) then 
                        if (timber >0) then
                            if (buo_index >0 ) then 
                                fload=1.75*alog10( timber ) + .32*alog10( buo_index ) - 1.640
                                if (fload >0) then
                                    fload = 10. ** fload
                                    return
                                end if 
                                fload = 0.
                                return
                            end if
                            return
                        end if
                        return
                    end if
                    timber  = 1.
                    if ((grass - 1.) <0) then
                        grass = 1.
                    end if
                    if (timber >0) then
                        if (buo_index >0 ) then 
                            fload=1.75*alog10( timber ) + .32*alog10( buo_index ) - 1.640
                            if (fload >0) then
                                fload = 10. ** fload
                                return
                            end if 
                            fload = 0.
                            return
                        end if
                        return
                    end if
                    return
                end if
                grass  = .00918*(wind+14.) * (33.-ffuelm)**1.65 - 3.
                if ((grass - 99.) > 0) then
                    grass = 99.
                    if ((timber - 99.) >0)  then
                        timber = 99.
                    end if
                end if
                if (timber > 0) then
                    if (buo_index > 0) then 
                        fload=1.75*alog10( timber ) + .32*alog10( buo_index ) - 1.640
                        if (fload >0) then
                            fload = 10. ** fload
                            return
                        else if((fload == 0 ).or. (fload < 0 )) then
                            fload = 0.
                            return
                        end if
                    end if
                    return
                end if
                return
            else if (((ffuelm - 30.) > 0) .or. ((ffuelm - 30.) ==0)) then
                grass = 1.
                timber = 1.
                return
            end if
        else if (((adfuelm -30.) < 0)) then 
            !see if the wind speed is greater than 14 mph
            if ((wind - 14. ) < 0) then 
                timber = .01312*(wind+6.) * (33.-adfuelm)**1.65 - 3.
                grass = .01312*(wind+6.) * (33.-ffuelm)**1.65 - 3.
                if ((timber - 1.) >0 ) then 
                    if (timber > 0) then
                        if (buo_index > 0) then 
                            fload=1.75*alog10( timber ) + .32*alog10( buo_index ) - 1.640
                            if (fload >0) then
                                fload = 10. ** fload
                                return
                            else if((fload == 0 ).or. (fload < 0 )) then
                                fload = 0.
                                return
                            end if
                        end if
                        return
                    end if
                    return
                end if
                timber = 1.
                if ((grass - 1.) <0) then 
                    grass = 1.
                else if (((grass - 1.) ==0) .or. ((grass - 1.) >0)) then
                    if (timber >0) then 
                        if (buo_index>0)then 
                            fload=1.75*alog10( timber ) + .32*alog10( buo_index ) - 1.640 
                            !ensure that fload >0 else set it to 0
                            if (fload >0) then
                                fload = 10. ** fload
                            end if
                            fload =0.
                            return
                        end if
                        return
                    end if
                    return
                end if
                !we set to grass = 1. now we are moving on
                if (timber >0) then 
                    if (buo_index>0)then 
                        fload=1.75*alog10( timber ) + .32*alog10( buo_index ) - 1.640 
                        !ensure that fload >0 else set it to 0
                        if (fload >0) then
                            fload = 10. ** fload
                        end if
                        fload =0.
                        return
                    end if
                    return
                end if
                return
            end if
            timber = .00918*(wind+14.) * (33.-adfuelm)**1.65 - 3.
            grass  = .00918*(wind+14.) * (33.-ffuelm)**1.65 - 3.
            if ((grass - 99.) >0) then
                grass = 99.
                if ((timber -99.) >0) then
                    timber = 99.
                end if
            end if
            if (timber >0) then 
                if (buo_index >0) then
                    fload=1.75*alog10( timber ) + .32*alog10( buo_index ) - 1.640 
                    if (fload >0) then
                        fload = 10. ** fload
                        return
                    end if
                    fload = 0.
                    return
                end if
                return
            end if
            return
        end if
    !if there is snow on the ground isnow == 1
    else if (isnow == 1) then
        !if there is snow, grass and timber indexes must be set to 0
        grass =0.
        timber =0.
        if ((precip - .1)== 0 .or. (precip - .1)<0) then
            return
        else if((precip - .1) > 0) then
            !precipitation exceeded .1 inches and we reduce the buildup index
            buo_index = -50.*alog(1.-(1.-exp(-buo_index/50.))*exp( -1.175*(precip-.1)))
            if ((buo_index == 0) .or. (buo_index>0)) then
                return
            else if (buo_index < 0) then
                buo_index = 0.
                return
            end if
        end if
    end if

end subroutine danger



!subroutine to get user input for snow and wind speed
subroutine input(drybulb,wetbulb,isnow,precip,wind,buo_index,herbState)
    implicit none 

    real :: drybulb, wetbulb, precip, wind, buo_index
    integer :: isnow, herbState

    drybulb = 50.0000000
    wetbulb = 34.0000000
    buo_index = 8.00000000
    precip = 0.449999988
    herbState = 1
    write (*,*) 'INPUT: '
    write (*,*) ' Dry bulb temperature = ', drybulb
    write (*,*) ' Wet bulb temperature = ', wetbulb
    write (*,*) ' Is there snow? = '
    !GET THE SNOW INPUT FROM THE USER
    read (*,*) isnow
    write (*,*) ' Is there snow? = ', isnow
    write (*,*) ' Wind speed mph? = '
    !GET THE WIND SPEED FROM THE USER
    read (*,*) wind
    write (*,*) ' Wind speed mph? = ', wind
    write (*,*) ' Build up index = ', buo_index
    write (*,*) ' Herb state = ', herbState
    write (*,*) ' Precipitation = ', precip

end subroutine input

!Output of calculated info from DANGER
subroutine output(buo_index,ffuelm,adfuelm,timber,fload, grass)
    implicit none 
    real :: ffuelm, adfuelm, timber, fload, buo_index, grass

    write (*,*) 'OUTPUT: '
    write (*,*) ' Fine Fuel Moisture = ', ffuelm 
    write (*,*) ' Adjusted Fuel Moisture = ', adfuelm
    write (*,*) ' Fine Fuel Spread = ', grass
    write (*,*) ' Timber Spread Index = ', timber
    write (*,*) ' Fire Load Index = ', fload
    write (*,*) ' Build Up Index = ', buo_index

end subroutine output
