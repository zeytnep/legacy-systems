-- hangman.adb 
-- Hangman game in ada for CIS*3190 - Assignment 2
-- @author Zeynep Erdogru (zerdogru) student ID#1047085

--Assumptions:
-- No uper case letters allowed

with ada.Text_IO; use Ada.Text_IO;
with ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with ada.Numerics.Discrete_Random;
with ada.Strings.Maps; use ada.Strings.Maps;

procedure Hangman is

    type my_range is range 1..50;
    package Rand_Index is new ada.Numerics.Discrete_Random(my_range);
    use Rand_Index;
    gen : Rand_Index.Generator;

    type image_type is array(1..12) of string(1..12);
    hanging_man : image_type;

    guesses : string(1..26);
    hidden : string(1..20);

    letter_guess : character := ' ';
    flag : integer := 1;
    word_length : integer := 0;
    word : string(1..20);
    
    word_guess : string(1..20);

    word_guess_length : integer;
    hidden_length : integer;
    n_guesses : integer := 0;
    num_wrong : integer := 0;
    num_found : integer := 0;

    used : array(my_range) of Boolean := (my_range => false);
    random_number : my_range;

    dictionary_lengths : constant array(my_range) of integer := (
        3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5,
        6, 6, 6, 6, 6, 6, 7, 7, 7, 6, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9,
        9, 10, 11, 11, 10, 11, 19, 13);

    dictionary : constant array(my_range) of string(1..20) := (
        "gum                 ", "sin                 ", "for                 ",
        "cry                 ", "lug                 ", "bye                 ",
        "fly                 ", "ugly                ", "each                ",
        "from                ", "work                ", "talk                ",
        "with                ", "self                ", "pizza               ",
        "thing               ", "feign               ", "fiend               ",
        "elbow               ", "fault               ", "dirty               ",
        "budget              ", "spirit              ", "quaint              ",
        "maiden              ", "escort              ", "pickax              ",
        "example             ", "tension             ", "quinine             ",
        "kidney              ", "replica             ", "sleeper             ",
        "triangle            ", "kangaroo            ", "mahogany            ",
        "sergeant            ", "sequence            ", "moustache           ",
        "dangerous           ", "scientist           ", "different           ",
        "quiescent           ", "magistrate          ", "erroneously         ",
        "loudspeaker         ", "phytotoxic          ", "matrimonial         ",
        "parasympathomimetic ", "thigmotropism       ");

    -- Determine the users wish. Prompt user and ask her/him if she/he wants to play another round
    function yes (flag: in out integer) return boolean is
        ch: character;
    begin
        flag := flag + 1;
        put_line("Do you want another word? (Y/N) ");
            loop
                get(ch);
                skip_line;
                case ch is
                    when 'y' | 'Y' =>
                        return true;
                    when 'n' | 'N' =>
                        put_line("It's been fun! Bye for now.");
                        put_line("Ending...");
                        return false;
                    when others =>
                        put("please type y for yes OR n for no: ");
                end case;
            end loop;
    end yes;

    -- Initialize some variables and the game image of hanging_man
    procedure init_game (hanging_man : in out image_type; hidden: in out string; guesses: in out string; num_wrong: in out integer; n_guesses: in out integer) is 
    begin
        hanging_man(1) := (1..7 => 'X') & (1..5 => ' ');
        for i in 2..12 loop
            hanging_man(i)(1) := 'X';
            hanging_man(i)(2..12) := (2..12 => ' ');
        end loop;
        hanging_man(2)(7) := 'X';

        hidden := (1..20 => '-');
        guesses := (1..26 => ' ');
        num_wrong := 0;
        n_guesses := 0;
    end init_game;

    --display used letters
    procedure display_used_letters(n_guesses: in integer; guesses: in string) is 
    begin 
        put("Here are the letters you used: ");
        for i in 1..n_guesses loop
            put(guesses(i));
            put(',');
        end loop;
        put_line(" ");
    end display_used_letters;

    -- Loops until it gets a "valid" character from the user
    -- upper case letters are NOT valid
    procedure get_letter (letter_guess: in out character; n_guesses: in integer; guesses: in string) is
        cha: character;
    begin 
        put_line("What is your guess? ");
        checker: loop
            get(cha);
            skip_line;
            case cha is
                when 'q'|'w'|'e'|'r'|'t'|'y'|'u'|'i'|'o'|'p'|'a'|'s'|'d'|'f'|'g'|'h'|'j'|'k'|'l'|'z'|'x'|'c'|'v'|'b'|'n'|'m' =>
                    exit checker;
                when 'Q'|'W'|'E'|'R'|'T'|'Y'|'U'|'I'|'O'|'P'|'A'|'S'|'D'|'F'|'G'|'H'|'J'|'K'|'L'|'Z'|'X'|'C'|'V'|'B'|'N'|'M' =>
                    put_line("Invalid character. Please use lower case letters.");
                    display_used_letters(n_guesses, guesses);
                    put_line("What is your guess? ");
                when others =>
                    put_line("Invalid character");
                    display_used_letters(n_guesses, guesses);
                    put_line("What is your guess? ");
            end case;
        end loop checker;

        letter_guess := cha;
    end get_letter;

    -- Update the image of hanging_man
    procedure hang_the_man (num_wrong: in integer; hanging_man: in out image_type) is
    begin
        if num_wrong = 1 then
            put_line("First we draw a head.");
            hanging_man(3)(6) := '-';
            hanging_man(3)(7) := '-';
            hanging_man(3)(8) := '-';
            hanging_man(4)(5) := '(';
            hanging_man(4)(6) := '.';
            hanging_man(4)(8) := '.';
            hanging_man(4)(9) := ')';
            hanging_man(5)(6) := '-';
            hanging_man(5)(7) := '-';
            hanging_man(5)(8) := '-';
        end if;

        if num_wrong = 2 then 
            put_line("Now we draw a body.");
            hanging_man(6)(7) := 'X'; 
            hanging_man(7)(7) := 'X'; 
            hanging_man(8)(7) := 'X'; 
            hanging_man(9)(7) := 'X'; 
        end if;

        if num_wrong = 3 then
            put_line("Next we draw an arm.");
            hanging_man(4)(3) := '\'; 
            hanging_man(5)(4) := '\'; 
            hanging_man(6)(5) := '\'; 
            hanging_man(7)(6) := '\'; 
        end if;

        if num_wrong = 4 then 
            put_line("This time it's the other arm.");
            hanging_man(4)(11) := '/';
            hanging_man(5)(10) := '/';
            hanging_man(6)(9) := '/';
            hanging_man(7)(8) := '/';
        end if;

        if num_wrong = 5 then 
            put_line("Now, let's draw the right leg.");
            hanging_man(10)(6) := '/';
            hanging_man(11)(5) := '/';
        end if;

        if num_wrong = 6 then
            put_line("This time we draw the left leg.");
            hanging_man(10)(8) := '\';
            hanging_man(11)(9) := '\';
        end if;

        if num_wrong = 7 then
            put_line("Now we put up a hand.");
            hanging_man(3)(11) := '\';
        end if;

        if num_wrong = 8 then
            put_line("Next the other hand.");
            hanging_man(3)(3) := '/';
        end if;

        if num_wrong = 9 then
            put_line("Now we draw one foot");
            hanging_man(12)(10) := '\';
            hanging_man(12)(11) := '-';
        end if;

        if num_wrong = 10 then
            put_line("Here's the other foot.");
            new_line;
            put_line("You're hung!!");
            hanging_man(12)(4) := '-';
            hanging_man(12)(3) := '\';
        end if;

        -- Draw the man
        for i in 1..12 loop
            put_line(hanging_man(i));
        end loop;

    end hang_the_man;


    procedure replace_dashes(num_found: in out integer; word_length: in integer; word: in string; hidden: in out string; letter_guess: in character) is
    begin
        num_found := 0;
            for i in 1..word_length loop
                if word(i) = letter_guess then
                    hidden(i) := letter_guess;
                    num_found := num_found + 1;
                end if;
            end loop;
    end replace_dashes;
  
begin

    put_line("THE GAME OF HANGMAN");

    while flag <= 50 loop

        -- PROCEDURE CALL
        -- Initialize the game image and variables by calling init_game procedure
        init_game(hanging_man, hidden, guesses, num_wrong, n_guesses);

        -- Get random word from dictionary
        loop
            reset(gen);
            random_number := Random(gen);
            exit when used(random_number) = false;
        end loop;
        used(random_number) := true;
        word := dictionary(random_number);
        word_length := dictionary_lengths(random_number);
        hidden_length := word_length;


        -- Print number of dashes for hidden word
        put_line(hidden(1..hidden_length));

        while num_wrong <= 10 loop

            -- PROCEDURE CALL
            -- Print out guessed letters
            display_used_letters(n_guesses, guesses);

            --if user made 10 mistakes, exit
            if num_wrong = 10 then
                put_line("Sorry, you lose. The word was ->" &  word(1..word_length));
                put_line("You missed that one.");
                new_line;
                exit;
            end if;

            -- PROCEDURE CALL
            -- Get next letter guess
            get_letter(letter_guess, n_guesses, guesses);

            -- Check if the letter has been guessed already
            if Is_In(letter_guess, To_Set(guesses)) then
                put_line("You have guessed that letter before.");
            else
                n_guesses := n_guesses + 1;
                guesses(n_guesses) := letter_guess;
                
                -- PROCEDURE CALL
                -- Replace dashes in hidden with matching letter
                replace_dashes(num_found, word_length, word, hidden, letter_guess);

                -- Print updated image if letter_guess wasn't a match
                if num_found = 0 then
                    num_wrong := num_wrong + 1;
                    put_line("Sorry, that letter isn't in the word.");

                    -- PROCEDURE CALL
                    -- Update the image
                   hang_the_man(num_wrong, hanging_man);

                elsif Is_In('-', To_Set(hidden(1..hidden_length))) then

                    -- Check users guess for the solution
                    put_line(hidden(1..hidden_length));
                    put_line("What is your guess for the word?");
                    get_line(word_guess, word_guess_length);

                    if word_length = word_guess_length and
                            word(1..word_length) = word_guess(1..word_guess_length) then
                        put("That's correct!:) It took you ");
                        put(n_guesses);
                        put_line(" guesses.");
                        new_line;
                        exit;
                    else
                        put_line("Wrong. Try another letter");
                    end if;
                else
                    put_line("You found all the letters in the word!");
                    exit;
                end if;
            end if;
        end loop;

        -- If player failed to guess the word, print a sorry message and let them know what the word was
        if num_wrong = 11 then
            put_line("Sorry, you lose. The word was -->" &  word(1..word_length));
            put_line("You missed that one.");
            new_line;
        end if;

        -- PROCEDURE CALL
        -- Prompt user for another round
        exit when not yes(flag);

    end loop;

    -- If user completed every word
    if flag = 51 then
        put_line("You did all the words.");
    end if;

end Hangman;
