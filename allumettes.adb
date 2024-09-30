with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Alea;

procedure Allumettes is

    -- Represents either the computer or the human.
    type Player is (Human, Computer);
    -- Represents at which level the computer will be playing 
    type Computer_Level is (naif, distrait, rapide, expert);
    type Game_State is record
        -- Represents how many matches are left
        Matches : Integer;
        Next_Player : Player;
        Level : Computer_Level;
    end record;
    
   -- Returns a Computer_Level corresponding to user input.
   --
   -- Parameters :
   --    N/A
   -- Return :
   --	The Computer_Level at which the computer will be playing.
    function Get_Level return Computer_Level is
        A : Character;
        Res : Computer_Level;
    begin
        Put_Line("Quel doit être le niveau de l'ordinateur ? (n, d, f, e)");
        Get(A);
        case A is
            when 'n' | 'N' => Res := naif;
            when 'd' | 'D' => Res := distrait;
            when 'f' | 'F' => Res := rapide;
            when others => Res := expert;
        end case;
        Put_Line("Mon niveau est "& Computer_Level'Image(Res) & ".");
        return Res;
    end Get_Level;
    
   -- Returns a Player (Human/Computer) corresponding to user input.
   --
   -- Parameters :
   --    N/A
   -- Return :
   -- 	 The First player to begin the game.
    function Get_First_Player return Player is
        A : Character;
    begin
        Put_Line("Est-ce que vous commencez (o/n) ?");
        Get(A);
     
        case A is
            when 'o' | 'O' => return Human;
            when others => return Computer;
        end case;
    end Get_First_Player;

   -- Creates and returns a new Game_State which corresponds to game's beginning.
   --
   -- Parameters :
   --    N/A
   -- Return :
   --	A new initial Game_State
    function Initialize_Game_State return Game_State is
        Level : Computer_Level;
        First_Player : Player;
    begin
        Level := Get_Level;
        First_Player := Get_First_Player;
        return (Next_Player => First_Player, Matches => 13, Level => Level);
    end Initialize_Game_State;

    Game : Game_State;
    Next_Move : Integer;

   -- Displays the remaining number of matches in the game.
   --
   -- Parameters :
   --    Matches	:	The number of matches to display.
   -- Retour :
   -- 	 Néant
    procedure Display_Game(Matches : in Integer) is
    	-- Used to avoid directly modifying the "Matches" variable.
    	Matches_Clone : Integer;
	Loop_Counter : Integer;
	Res : Unbounded_String := To_Unbounded_String("");
    begin
    	Matches_Clone := Matches;
    	Loop_Counter := 0;
    	while Matches_Clone > 0 loop
    		Append(Res, "| ");
    		Loop_Counter := Loop_Counter + 1;
    		-- Group matches by 5
    		if Loop_Counter mod 5 = 0 then
    			Append(Res, "   ");
    		end if;
    		Matches_Clone := Matches_Clone - 1;
    	end loop;
    	Put_Line(To_String(Res));
    	Put_Line(To_String(Res));
    	Put_Line(To_String(Res));
	New_Line;
	New_Line;
    end Display_Game;

   -- Ask the user how many matches he wants to take.
   --
   -- Parameters :
   --    N/A
   -- Return :
   -- 	 An Integer between 1 & 3
   
    function Ask_Move return Integer is
        A : Integer;
    begin
    	Put_Line("Combien d'allumettes prenez-vous ?");
    	Get(A);
    	return A;
    end Ask_Move;

   -- Verify if a move is valid or not.
   --
   -- Parameters :
   --    State :	The actual State of the Game  (Game_State type).
   -- Return :
   -- 	 A Boolean indicating if the move is valid or not.Random
   -- Assert :
   -- 	 Is_Valid'Result >= 1 and Is_Valid'Result <= Min(3,State.Matches)
    function Is_Valid(State : in Game_State; Move : in Integer) return Boolean is
    begin
    	if State.Matches < Move then
    		Put_Line("Arbitre : Il reste seulement " & Integer'Image(State.Matches) & " allumettes.");
    		return false;
    	end if;
    	if Move < 1 then
    		Put_Line("Arbitre : Il faut prendre au moins une allumette.");
    		return false;
    	end if;
    	
    	if Move > 3 then
    		Put_Line("Arbitre : Il est interdit de prendre plus de 3 allumettes.");
    		return false;
    	end if;
    	
	return True;
    end Is_Valid;
    
    -- Computes the minimum of two integers.
    -- Parameters :
    --  A : in Integer
    --  B : in Integer
    -- Return : Integer -- the minimum of A and B.
    -- Assert :
    --      Min'Result <= A
    --	    Min'Result <= B
    function Min(A : in Integer; B : in Integer) return Integer is
    begin
    	if A >= B then 
    		return B;
    	else
    		return A;
    	end if;
    end Min;
    
    -- Ask the next player how many matches he will take as long as he doesn't provide a valid input.
    -- Parameters :
    --  State : in Game_State the actual state of the game
    -- Return : Integer -- a valid number of matches to take.
    -- Assert :
    --      Get_Next_Move'Result >= 1
    --	    Get_Next_Move'Result <= 3
    --	    Get_Next_Move'Result <= State.Matches
    function Get_Next_Move(State : in Game_State) return Integer is
        A : Character;
        Res : Integer;
        Looped : Boolean;
        package Mon_Alea is
		new Alea (1, 3);  -- générateur de nombre dans l'intervalle [1, 3]
	use Mon_Alea;
	Nombre : Integer;

    begin
        
        Looped := False;
    	Res := 1;
    	while (Looped and Is_Valid(State, Res)) = False loop
    		if State.Next_Player = Human then
        		Res := Ask_Move;	
        	else
        		Get_Random_Number(Nombre);
        		case State.Level is
        			when naif => 
        				while Nombre > Game.Matches loop
        					Get_Random_Number(Nombre);
        				end loop;
        				Res := Nombre;
        			when distrait => Res := Nombre;
        			when rapide => Res := Min(Game.Matches,3);
        			when expert => 
        				case State.Matches mod 4 is
        					when 3 => Res := 2;
        					when 2 | 1 => Res := 1;
        					when others => Res := 3;
        				end case;				
        		end case;
        	end if;
        	Looped := True;
        end loop;
        return Res; 

    end Get_Next_Move;
    
begin
    Game := Initialize_Game_State;
   
    -- Game main loop
    while Game.Matches > 0 loop
    	Display_Game(Game.Matches);
    	-- Get the next move that will be played.
    	Next_Move := Get_Next_Move(Game);    	  
    	-- Updates the remaining number of matches
    	Game.Matches := Game.Matches - Next_Move;    	
    	-- Updates next_player variable
    	if Game.Next_Player = Computer then 
    		Game.Next_Player := Human;
    		Put_Line("Je prends " & Integer'Image(Next_Move) & " allumettes.");
    	else
    		Game.Next_Player := Computer;
    		Put_Line("Vous prenez " & Integer'Image(Next_Move) & " allumettes.");
    	end if;
    end loop;
    
    if Game.Next_Player = Computer then Put("J'ai gagné.");
    else Put("Vous avez gagné, félicitations.");
    end if;
end Allumettes;
