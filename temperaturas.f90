IMPLICIT NONE

! 01/02/2013
 
REAL TEMP(109, 12), DEC(11), MONTH(12), YEAR(109), MIN, MAX, EST(4)
INTEGER I, J, COUNT, ER
 
DATA DEC /11*0.0/, MONTH /12*0.0/, YEAR /109*0.0/, MIN /0.0/, MAX /0.0/, EST /4*0.0/, COUNT /0/
 
OPEN(UNIT=31154, FILE="temprio.txt")

DO I=1, 109
        DO J=1, 12
                READ(31154, *) TEMP(I, J)
! Verifica se a temperatura lida é menor que a última mínima encontrada
                IF ((I==1 .AND. J==1) .OR. (TEMP(I,J) < MIN)) THEN
                        MIN = TEMP(I, J)
                ENDIF
! Verifica se a temperatura lida é maior que a última máxima encontrada
                IF ((I==1 .AND. J==1) .OR. (TEMP(I,J) > MAX)) THEN
                        MAX = TEMP(I, J)
                ENDIF
        ENDDO
ENDDO
 
! Calcula a soma de todas as temperaturas nos anos e meses (Média tirada na próxima etapa)
do i=1, 109
        do j=1, 12
                month(j) = month(j) + temp(i,j)
                year(i) = year(i) + temp(i, j)
        enddo
enddo
 
! Calcula e imprime as médias mensais na tela
write(*, '(A)') "Médias mensais:"
do i=1,12
        month(i) = month(i)/109
        write(*, '(A, I2, A, F5.2)'), "Mês ", i, ": ", month(i)
enddo
 
!Calcula e imprime as médias das estações na tela
do i=1,109
        do j=1,12

                if (j <= 2 .OR. j == 12) then
                        est(1) = est(1) + temp(i, j)
                elseif (j >= 3 .AND. j <= 5) then
                        est(2) = est(2) + temp(i, j)
                elseif (j >= 6 .AND. j <= 8) then
                        est(3) = est(3) + temp(i, j)
                elseif (j >= 9 .AND. j <= 11) then
                        est(4) = est(4) + temp(i, j)
                endif
        enddo
enddo
 
write(*, '(/, A)') "Médias das estações:"
do i=1,4
        est(i) = est(i)/(3*109)
        if (i == 1) then
                write(*, '(A, F5.2)'), "Verão: ", est(1)
        endif
        if (i == 2) then
                write(*, '(A, F5.2)'), "Outono: ", est(2)
        endif
        if (i == 3) then
                write(*, '(A, F5.2)'), "Inverno: ", est(3)
        endif
        if (i == 4) then
                write(*, '(A, F5.2)'), "Primavera: ", est(4)
        endif
enddo
 
! Calcula e imprime as médias anuais na tela
write(*, '(/, A)') "Médias anuais:"
do i=1,109
        year(i) = year(i)/12
        write(*, '(A, I3, A, F5.2)'), "Ano ", i, ": ", year(i)
enddo
 
!Calcula e imprime as médias das décadas na tela
j = 1
write(*, '(/, A)') "Médias das décadas:"
do i=1, 109
        dec(j) = dec(j) + temp(i, j)
        count = count + 1
        if (count >= 10) then
                dec(j) = dec(j)/count
                write(*, '(A, I2, A, F5.2)'), "Década ", j, ": ", dec(j)
                j = j + 1
                count = 0
        endif
enddo
! Caso da última média que é feita com menos anos que as outras
dec(j) = dec(j)/count
write(*, '(A, I2, A, F5.2)'), "Década ", j, ": ", dec(j)
 
!Imprime a máxima e a mínima
write(*, '(/, A, F5.2)'), "Temperatura mínima: ", min
write(*, '(A, F5.2)'), "Temperatura máxima: ", max
 
end