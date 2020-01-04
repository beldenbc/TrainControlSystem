package body Layout.Search is
   ----------------------------------------------------------------------------
   -- Consider using this as a local recursive search procedure ...
   ----------------------------------------------------------------------------
   procedure Search (Start     : in     Block_ID;       -- Rear of train
                     Finish    : in     Block_ID;       -- Front of train
                     Direction : in     Block_Polarity; -- Search direction
                     Blocks    : in out Block_List;     -- Under train
                     Turnouts  : in out Turnout_List;   -- Under train
                     Success   :        out Boolean) is

      -- Local variables will go here
      Turnout_Num : Turnout_ID;
      Turnouts_Added : Integer := 0;
      Hall_Num : Hall_ID;
      Next_Direction : Block_Polarity;

   begin
      --Base Case 0: Blocks list is full
      if Blocks.Size = Blocks.Max_Size then
         Success := false;
         return;
      end if;

      Blocks.Size := Blocks.Size + 1;
      Blocks.Items( Blocks.Size ) := ( Start, Direction );

      --Base Case 1: Start =  Finish
      if Start = Finish then
         Success := True;
         return;
      end if;

      case Terminated_By (Start, Direction) is
         --when a deadend (remove block)
         when A_Deadend =>
            Success := False;
            Blocks.Size := Blocks.Size - 1;
            return;

         --when a block (No choice on next step)
         when A_Block =>

            --when a reversing point
            Hall_Num := Next_Hall(Start, Direction);
            if Is_Reversing(Hall => Hall_Num) then
               Next_Direction := Opposite(Direction);
            end if;

            --force turnout
            if Has_Force_Turnout (Start, Direction) then
               declare
                  Turn_Num : Turnout_ID;
                  Turn_Limb : Turn_Choice;
               begin
                  Turnouts.Size := Turnouts.Size + 1;
                  Get_Force_Turnout (Block              => Start,
                                     Direction          => Direction,
                                     Turnout            => Turn_Num,
                                     Required_Direction => Turn_Limb);
                  Turnouts.Items (Turnouts.Size) := (Turn_Num, Turn_Limb);
                  Turnouts_Added := 1;
               end;
            end if;

            Search (Start  => Next_Block (Start, Direction),
                    Finish => Finish,
                    Direction => Next_Direction,
                    Blocks => Blocks,
                    Turnouts => Turnouts,
                    Success => Success);

            if Success then
               return;
            end if;

            Turnouts.Size := Turnouts.Size - Turnouts_Added;

         --when a turnout
         when A_Turnout =>
            Turnout_Num := Adjacent_Turnout (Start, Direction);

            for Limb in Turn_Choice loop
               Turnouts.Size := Turnouts.Size + 1;
               Turnouts.Items (Turnouts.Size) := (Turnout_Num, Limb);
               Turnouts_Added := 1;

               Hall_Num := Next_Hall(Turnout_Num, Limb);
               if Is_Reversing(Hall => Hall_Num) then
                  Next_Direction := Opposite(Direction);
               end if;

               --joint turnout
               if Has_Joint (Turnout_Num, Limb) then
                  Turnouts.Size := Turnouts.Size + 1;
                  Turnouts.Items (Turnouts.Size) := (Joint_Turnout (Turnout_Num, Limb), Limb);
                  Turnouts_Added := 2;
               end if;

               Search (Start => Next_Block (Turnout_Num, Limb),
                       Finish => Finish,
                       Direction => Next_Direction,
                       Blocks => Blocks,
                       Turnouts => Turnouts,
                       Success => Success);

               if Success then
                  return;
               end if;

               Turnouts.Size := Turnouts.Size - Turnouts_Added;
            end loop;

      end case;

      --No success (remove block)
      if Success = False then
         Blocks.Size := Blocks.Size - 1;
      end if;

   end Search;

   ----------------------------------------------------------------------------
   procedure Blocks_Beneath (Loco     : in  Block_ID;
                             Caboose  : in  Block_ID;
                             Blocks   : out Block_List;
                             Turnouts : out Turnout_List;
                             Success  : out Boolean) is
   begin
      -- First, try searching in the normal direction
      Search ( Start => Caboose,
               Finish => Loco,
               Direction => Normal,
               Blocks => Blocks,
               Turnouts => Turnouts,
               Success => Success);

      -- Next, try searching in the reverse direction
      if not Success then
         Blocks.Size := 0;
         Turnouts.Size := 0;

         Search ( Start => Caboose,
                  Finish => Loco,
                  Direction => Reversed,
                  Blocks => Blocks,
                  Turnouts => Turnouts,
                  Success => Success);
         end if;

   end Blocks_Beneath;

end Layout.Search;
