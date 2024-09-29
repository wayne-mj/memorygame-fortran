module cardmod
    implicit none

    ! Card type definition
    type :: cardtype
        character(len=1) :: value
        character(len=1) :: suit
        character(len=1) :: colour
        logical :: matched = .false.
        logical :: shuffled = .false.
    end type cardtype

    character(len=1), dimension(13) :: card_values = ['A', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K']
    character(len=1), dimension(4) :: card_suits = ['H', 'D', 'C', 'S']
    
    contains

    ! Add cards to the deck
    function populate_deck() result(thedeck)
        type (cardtype), dimension(52) :: thedeck        
        integer :: i, j, k
        
        i = 1               ! This is for the suits
        j = 1               ! This is for the values
        k = 1               ! This is for the deck  

        do i = 1, 4
            do j = 1, 13
                thedeck(k)%value = card_values(j) 
                thedeck(k)%suit = card_suits(i)
                if (card_suits(i) == 'H' .or. card_suits(i) == 'D') then
                    thedeck(k)%colour = 'R'
                else
                    thedeck(k)%colour = 'B'
                end if
                k = k + 1
            end do
        end do
    end function populate_deck

    ! Shuffle the deck
    function shuffle_deck(thedeck) result(shuffled_deck)
        type (cardtype), dimension(52) :: thedeck, shuffled_deck
        type (cardtype) :: temp_card
        integer :: i 
        integer :: random_card
        integer :: seed(8)
        real :: rand_value

        call random_seed()
        call system_clock(count=seed(1))
        call random_seed(put=seed)

        !call srand(1)

        do i=1, 52
            !random_card = int(rand()*52.0) + 1
            call random_number(rand_value)
            random_card = int(rand_value*52.0) + 1
            temp_card = thedeck(random_card)
            do while (temp_card%shuffled .eqv. .true.)
                !random_card = int(rand()*52.0) + 1
                call random_number(rand_value)
                random_card = int(rand_value*52.0) + 1
                temp_card = thedeck(random_card)
            end do
            thedeck(random_card)%shuffled = .true.
            temp_card%shuffled = .true.
            shuffled_deck(i) = temp_card
        end do

    end function shuffle_deck

    ! Reset the shuffle status flag of the deck
    subroutine reset_shuffle_flag(thedeck) 
        type (cardtype), dimension(52) :: thedeck
        integer :: i

        do i = 1, 52
            thedeck(i)%shuffled = .false.
        end do
    end subroutine reset_shuffle_flag

    ! Reshuffle the deck
    subroutine reshuffle(thedeck)
        type (cardtype), dimension(52) :: thedeck
        call reset_shuffle_flag(thedeck)
        thedeck = shuffle_deck(thedeck)
    end subroutine reshuffle

    ! Shuffle the deck n times
    subroutine shuffle(n, thedeck)
        type (cardtype), dimension(52) :: thedeck
        integer :: i, n

        do i=1,n
            call reshuffle(thedeck)
        end do
    end subroutine shuffle

    ! Create the board
    function create_board(thedeck) result(board)
        type(cardtype), dimension(52) :: thedeck
        type(cardtype), dimension(13,4) :: board
        integer :: i, j, k

        k = 1           ! This is for the deck

        do i = 1, 4
            do j = 1, 13
                board(j,i) = thedeck(k)
                k = k + 1
            end do
        end do
    end function create_board

end module cardmod