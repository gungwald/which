with Ada.Text_IO;       	
with Ada.Command_Line;  	
with Ada.Environment_Variables;	
with Ada.Strings.Fixed;
with Ada.Directories;		
with Ada.Exceptions;
with Ada.Characters.Handling;
with GNAT.OS_Lib;
with Ada.Containers.Vectors;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Ada.Strings.Unbounded.Text_IO;
use  Ada.Strings.Unbounded.Text_IO;

procedure Which is

    package vect  is new  Ada.Containers.Vectors(Element_Type => Unbounded_String, Index_Type => Natural);
    package exc   renames Ada.Exceptions;
    package tio   renames Ada.Text_IO;
    package env   renames Ada.Environment_Variables;
    package fixed renames Ada.Strings.Fixed;
    package dirs  renames Ada.Directories;
    package chars renames Ada.Characters.Handling;
    package ubs   renames Ada.Strings.Unbounded;
    package args  renames Ada.Command_Line;
    package os    renames GNAT.OS_Lib;


    find_all         : Boolean := False;
    verbose          : Boolean := False;
    substring_search : Boolean := False;
    path_dirs        : vect.Vector;
    exts             : vect.Vector;
    path_sep         : Unbounded_String;


    function Get_Env(Var_Name : String) return Unbounded_String is
        Env_Var_Not_Found_Error : exception;
        e : exc.Exception_Occurrence;
    begin
        return to_unbounded_string(env.Value(Var_Name));
    exception
        when e : others =>
            raise Env_Var_Not_Found_Error with "Environment variable not found: " & Var_Name;
    end Get_Env;


    function lc(str : Unbounded_String) return Unbounded_String is
        lowered_str : Unbounded_String;
    begin
        lowered_str := to_unbounded_string("");
        for i in 1..length(str) loop
            ubs.append(lowered_str, chars.to_lower(ubs.element(str, i)));
        end loop;
        return lowered_str;
    end lc;


    function ends_with(s : Unbounded_String; suffix_to_match : Unbounded_String) return Boolean is
        ends_with : Boolean := false;
    begin
        if lc(tail(s, length(suffix_to_match))) = lc(suffix_to_match) then
            ends_with := true;
        end if;
        return ends_with;
    end Ends_With;


    function index(search_in : Unbounded_String; search_for : Unbounded_String; start_position : Positive) return Natural is
    begin
        return index(search_in, to_string(search_for), start_position);
    end index;



    function Is_Executable(file : dirs.Directory_Entry_Type) return Boolean is
        extension_matches : Boolean := false;

        procedure check_extension(ext : vect.Cursor) is
        begin
            if Ends_With(to_unbounded_string(dirs.simple_name(file)), vect.element(ext)) then
                extension_matches := true;
            end if;
        end check_extension;
            
    begin
        if vect.is_empty(exts) then
            return os.is_executable_file(dirs.full_name(file)); -- UNIX
        end if;
        vect.iterate(exts, check_extension'access);
        return extension_matches;
    end Is_Executable;


    procedure split(parts:out vect.Vector; splittor:in Unbounded_String; to_split:in Unbounded_String) is
        splittor_position : Natural;
        Start_From : Positive := Positive'First;
    begin
        vect.clear(parts);
        splittor_position := index(to_split, splittor, Start_From);
        while splittor_position > 0 loop
            vect.append(parts, unbounded_slice(to_split, Start_From, splittor_position - 1));
            Start_From := splittor_position + length(splittor);
            splittor_position := index(to_split, splittor, Start_From);
        end loop;
        if Start_From <= length(to_split) then
            vect.append(parts, unbounded_slice(to_split, Start_From, length(to_split)));
        end if;
    end Split;


    function dir_exists(d:Unbounded_String) return Boolean is
        dir_name : String(1..length(d)) := to_string(d);
    begin
        return dirs.exists(dir_name) and ( dirs.file_kind'pos(dirs.kind(dir_name)) = dirs.file_kind'pos(dirs.DIRECTORY) );
    end dir_exists;


    procedure Which(search_term : String) is
        
        procedure search_dir(dir:vect.Cursor) is

            procedure Check_File(dir_entry:dirs.Directory_Entry_Type) is
                matches : Boolean := false;
                substr_position : Natural;
            begin
                if Substring_Search then
                    substr_position := fixed.index(dirs.simple_name(dir_entry), search_term, 1);
                    if substr_position > 0 and is_executable(dir_entry) then
                        tio.put_line(dirs.Full_Name(dir_entry));
                    end if;
                elsif search_term = dirs.simple_name(dir_entry) and is_executable(dir_entry) then
                    tio.Put_Line(dirs.Full_Name(dir_entry));
                end if;
            end Check_File;
        
        begin
            if Verbose then
                put_line("Checking dir: " & vect.element(dir));
            end if;

            if dir_exists(vect.element(dir)) then
                dirs.search(to_string(vect.element(dir)), "", (others => True), Check_File'Access);
            else
                if Verbose then
                    Put_Line("Directory not found: " & vect.element(dir));
                end if;
            end if;
        end search_dir;

    begin
        vect.iterate(path_dirs, search_dir'access);
    end Which;

--
-- Main Program
--
    e : exc.Exception_Occurrence;
begin
    if env.exists("PATHEXT") then
        path_sep := to_unbounded_string(";");
        split(exts, path_sep, get_env("PATHEXT"));
    else
        path_sep := to_unbounded_string(":");
    end if;

    split(path_dirs, path_sep, get_env("PATH"));

    -- If there are some command line arguments
    if args.Argument_Count > 0 then
        for i in 1..args.Argument_Count loop
            if args.Argument(I) = "-a" then
                Find_All := True;
            elsif args.Argument(I) = "-v" then
                Verbose := True;
            elsif args.Argument(I) = "-s" then
                Substring_Search := True;
            else
                Which(args.Argument(I));
            end if;
        end loop;
    else
        tio.put_line("No arguments");
    end if;

exception
    when e : others =>
        tio.put_line(tio.Standard_Error, "Exception: " & exc.Exception_Message(e));
end Which;

