--********************************
-- CIS*3190 A4 - REVERSE POLISH in Ada
-- @author Zeynep Erdogru
-- 05.04.2021
-- Email zerdogru@uoguelph.ca
--Assumptions
--No spaces are allowed inputting an expression. example/ 'A/   B/  C' is not allowed
--********************************

with Ada.Text_IO; use Ada.Text_IO;
with Ada.strings.unbounded; use ada.strings.unbounded;
with Ada.strings.unbounded.Text_IO; use ada.strings.unbounded.Text_IO;
with Ada.integer_text_IO; use Ada.integer_text_IO;

procedure polish is
pLen : Integer;
length : Integer;
top : Integer;
currChar : character;
yesNo : character;
inputString: String(1 .. 40);
covertedExpression: String(1 .. 40);
stack : String(1 .. 40);
trashStr : String(1..100);

--Procedure Introduction.
--Purpose: Print out welcome messsage
   procedure introduction is
   begin
      put_line (" ");
      put_line ("------------------------------------");
      put_line ("  WELCOME TO REVERSE POLISH EXPRESSION CONVERTER! ");
      put_line ("  By: Zeynep Erdogru (1047085) ");
      put_line ("------------------------------------");
      put_line (" ");
   end introduction;

--Procedure Push.
--Purpose: Push item to the stack
   procedure push(sym:character) is
   begin
      if(top < 40) then
   	   top := top + 1;
   	   stack(top) := sym;
      else
         put_line("Stack overflow!");   
      end if;
   end push;

--Procedure Pop.
--Purpose: Pop item off the stack and append it to the output string
   procedure pop(sym:character) is
   begin
      if(top > 0) then
         pLen := pLen + 1;
         covertedExpression(pLen) := stack(top);
         top := top - 1;
      else
         put_line("Stack underflow");   
      end if;   
   end pop;

--Function Priority.
--Purpose: Return the priority of a specific symbol
   function priority(sym:character) return Integer is
   begin
      if sym = ')' then
         return -1;
      elsif sym = '%' then
         return -1;
      elsif sym = '(' then 
         return 0;
      elsif sym = '+' then 
         return 1;
      elsif sym = '-' then
         return 1;
      elsif sym = '*' then 
         return 2;
      elsif sym = '/' then 
         return 2;
      elsif sym = '^' then 
         return 3;
      else 		
			put_line("The symbol you entered is invalid");
			return -2;
      end if;
   end priority;

--Function Yes.
--Purpose: Determine the users wish. Prompt user and ask her/him if she/he wants to convert another expression
   function yes (yesNo: in out character) return boolean is
      ch: character;
   begin
      put_line("Do you want to convert another expression? (Y/N) ");
         loop
            get(ch);
            case ch is
               when 'y' | 'Y' =>
                  yesNo := 'Y';
                  return true;
               when 'n' | 'N' =>
                  put_line("GOODBYE...");
                  return false;
               when others =>
                  skip_line;
                  put("Please type 'Y' for yes OR 'N' for no: ");
            end case;
         end loop;
   end yes;

begin
   --PROCEDURE CALL
   --print welcome message 
   introduction;
   --initialize yesNo char to 'Y' at the beggining
   yesNo := 'Y'; 

   --Main loop of the program
   --Loops until user types 'n' or 'N' to exit (when prompted).
   while (yesNo = 'Y') loop
      pLen := 0;
      top := 1;
      stack(top) := '%';

      put_Line("(Please do not use spaces)");
      put_Line("Input an expression you want to convert: ");
      get_Line(inputString, length);

      --examine each character separately and do the converting 
      for i in 1..length loop
      		currChar := inputString(i);
      		case currChar is
      			when 'a'..'z' | 'A'..'Z' | '0'..'9' =>
      				pLen := pLen + 1;
      				covertedExpression(pLen) := currChar;
      			when '%' | '-' | '+' | '/' | '*' | '^' =>
         			loop
                     if(priority(currChar) > priority(stack(top))) then
         					push(currChar);
                        exit;
                     else
                        pop(currChar);    
         				end if;
                  end loop;

      			when '(' | ')' => 
      				if(currChar = '(') then
                     push(currChar);
                  else
                     while (priority(stack(top)) /= priority('(')) loop
                        pop(currChar);
                     end loop;   
                     top := top - 1;
                  end if;
      			when others => 
      				put_line("This operator is NOT valid!!");
      		end case;	
      end loop;

      while top > 1 loop
         case stack(top) is
            when '(' => 
               put_line("Unmatched bracket");
            when others =>
               pLen := pLen + 1;
               covertedExpression(pLen) := stack(top);
         end case;
         top := top - 1;
      end loop;

      for i in 1..pLen loop
      		put(covertedExpression(i));
      end loop;
      new_line;

      -- FUNCTION CALL
      -- Prompt user for another round
      exit when not yes(yesNo);
      --CLEAN the buffer
      get_Line(trashStr,length);

   end loop;
end polish;
