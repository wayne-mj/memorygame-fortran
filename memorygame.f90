program memorygame
    use cardmod
    implicit none
    integer :: i, j,k, n
    type(cardtype), dimension(52) :: deck
    type(cardtype), dimension(13,4) :: board

    n = 2
    k=1

    deck = populate_deck()

    call shuffle(n, deck)

    board = create_board(deck)
    do i = 1, 4
        do j = 1, 13
            write (*, '(A,A,A)', advance='no') board(j,i)%value, board(j,i)%suit, " "
        end do
        print *
    end do
end program memorygame