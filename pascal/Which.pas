program Which;

uses
    gpc;

type
    Vector = array[0..255] of String(128);

var
    ProgramToFind: String(1024);
    Path: String(1024);
    Directory: String;
    PathExt: String(1024);
    Extensions: Vector;
    ExtensionCount: Integer;
    i: Integer;
    someFile: String(1024);

(**
 * Destructively parses BeingTokenized.
 *)
function NextToken(var BeingTokenized: String;
                   Separator: String;
                   var Token: String): Boolean;
var
    Location: Integer;
    HasNextToken: Boolean;
begin
    Location := Index(BeingTokenized, Separator);
    if Location > 0 then begin
        HasNextToken := True;
        Token := SubStr(BeingTokenized, 1, Location - 1);
        BeingTokenized := SubStr(BeingTokenized, Location + Length(Separator));
    end else if Length(BeingTokenized) > 0 then begin
        HasNextToken := True;
        Token := BeingTokenized;
        BeingTokenized := '';
    end else begin
        HasNextToken := False;
    end;
    NextToken := HasNextToken;  (* Return whether there is another token *)
end;

(**
 * Split a String into parts and put the parts into a Vector.
 *)
function Split(SplittethMe: String; ThySplittor: Char; var Splitten: Vector): Integer;
var
    Count: Integer;
    Part: String(10);
begin
    Count := 0;
    while NextToken(SplittethMe, ';', Part) do begin
        Splitten[Count] := Part;
        Count := Count + 1;
    end;
    Split := Count;  (* Return how many parts it was split into *)
end;

procedure CorrectFileNameCase(var absFileName: String);
var
    parentDirectoryName: String(1024);
    parentDirectory: DirPtr;
    shortFileName: String(256);
    someFile: String(256);
    done: Boolean;
begin
    shortFileName := NameFromPath(absFileName) + ExtFromPath(absFileName);
    parentDirectoryName := DirFromPath(absFileName);
    parentDirectory := OpenDir(parentDirectoryName);
    done := false;
    while not done do begin
        someFile := ReadDir(parentDirectory);
        if StrEqualCase(someFile, shortFileName) then begin
            done := true;
            absFileName := parentDirectoryName + someFile;
        end;
    end;
end;

function isExecutable(var name: String): Boolean;
var
    fileToTest: bindable File;
    binder: BindingType;
    executable: Boolean;
begin
    executable := false;
    binder.Name := name;
    Bind(fileToTest, binder);
    binder := Binding(fileToTest);
    if binder.Existing then begin
        executable := true;
        CorrectFileNameCase(name);
    end;
    Unbind(fileToTest);
    isExecutable := executable;
end;

(* Main Program *)
begin
    if ParamCount <> 1 then begin
        Writeln('usage: which <program_name>');
        Halt(0);
    end;
    ProgramToFind := ParamStr(1);

    PathExt := GetEnv('PATHEXT');
    ExtensionCount := Split(PathExt, ';', Extensions);

    Path := GetEnv('PATH');
    while NextToken(Path, ';', Directory) do begin
        for i := 0 to ExtensionCount - 1 do begin
            someFile := Directory + '\' + ProgramToFind + Extensions[i];
            if isExecutable(someFile) then begin
                Writeln(someFile);
            end;
        end;
    end;
end.

