import pathlib
import re

current_script = pathlib.Path(__file__)
workspace = current_script.parent.resolve()
sources = pathlib.PurePath(workspace, "src")
inputs = pathlib.PurePath(workspace, "inputs")
generated_file = pathlib.PurePath(sources, "aoc-generated.adb")

implemented_days = set()
for source in pathlib.Path(sources).glob("aoc-day_*.adb"):
    m = re.match("aoc-day_(\\d+)\\.adb", source.name)
    if m is None:
        continue
    day_id = int(m.group(1))
    if day_id >= 1 and day_id <= 12:
        implemented_days.add(day_id)

part_one_cases = ""
part_two_cases = ""
day_imports = ""

for day_id in implemented_days:
    indent_1 = ' ' * 9
    indent_2 = ' ' * 12

    day_pkg = f"Aoc.Day_" + str(day_id)
    day_imports += f"with {day_pkg};"

    case_guard = indent_1 + "when " + str(day_id) + " =>\n"
    part_one_cases += case_guard
    part_two_cases += case_guard

    part_one_cases += f"{indent_2}{day_pkg}.Part_One (Input);\n"
    part_two_cases += f"{indent_2}{day_pkg}.Part_Two (Input);\n"

generated_code = f"""
with Ada.Text_IO; use Ada.Text_IO;
{day_imports}

package body Aoc.Generated is
   function Get_Input_Dir return String is
   begin
      return "{re.escape(inputs.as_posix())}";
   end Get_Input_Dir;

   procedure Run_Day_Part_One (Id : Day_Id; Input : String) is
   begin
      case Id is
{part_one_cases}
         when others =>
            Put_Line (Standard_Error, "Unimplemented day " & Id'Image);
            raise Program_Error;
      end case;
   end Run_Day_Part_One;

   procedure Run_Day_Part_Two (Id : Day_Id; Input : String) is
   begin
      case Id is
{part_two_cases}
         when others =>
            Put_Line (Standard_Error, "Unimplemented day " & Id'Image);
            raise Program_Error;
      end case;
   end Run_Day_Part_Two;
end Aoc.Generated;
"""

with open(generated_file, "w") as f:
    f.write(generated_code)
