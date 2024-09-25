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
    -- Which level will the computer be playing ?
    type Computer_Level is (Naive, Distracted, Fast, Expert);
    type Game_State is record
        -- How many matches are left ?
        Matches : Integer;
        Next_Player : Player;
        Level : Computer_Level;
    end record;

    package Alea_1_3 is new Alea(1, 3);
    use Alea_1_3;

    -- Getting a level from input
    function Get_Level return Computer_Level is
        A : Character;
    begin
        Put_Line("Quel doit être le niveau de l'ordinateur ? (n, d, f, e)");
        Get(A);
        case A is
            when 'n' | 'N' => return Naive;
            when 'd' | 'D' => return Distracted;
            when 'f' | 'F' => return Fast;
            when others => return Expert;
        end case;
        Put_Line("Mon niveau est " & "" & ".");
    end Get_Level;
    
    -- Getting the player that plays first.
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

    -- Initializing the Game_State at the beginning of the program
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
    
    procedure Display_Game(Matches : Integer) is
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


    function Ask_Move return Integer is
        A : Integer;
    begin
    	Put_Line("Combien d'allumettes prenez-vous ?");
    	Get(A);
    	return A;
    end Ask_Move;
    
    function Is_Valid(State : Game_State; Move : Integer) return Boolean is
    begin
    	if State.Matches < Move then
    		Put_Line("Arbitre : Il reste seulement " & Integer'Image(Move) & " allumettes.");
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
    
    
    -- How many matches will the next player choses ?
    function Get_Next_Move(State : Game_State) return Integer is
        A : Character;
        Res : Integer;
        Looped : Boolean;
    begin
        
        Looped := False;
    	Res := 1;
    	while (Looped and Is_Valid(State, Res)) = False loop
    		if State.Next_Player = Human then
        		Res := Ask_Move;	
        	else
        		case State.Level is
        			when Naive => Res := Get_Random_Number(1,Game.Matches)
        			when Distracted => Res := Get_Random_Number(1,3);
        			when Fast => Res := Min(Game.Matches,3);
        			when Expert => Res := 1;
        		end case;
        	end if;
        	Looped := True;
        end loop;
        return Res;
    end Get_Next_Move;
    
begin
    Game := Initialize_Game_State;
   
    package Mon_Alea is
    	new Alea
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
